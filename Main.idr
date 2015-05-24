-- Local Variables:
-- idris-packages: ("lightyear" "contrib")
-- End:

import Lightyear.Strings
import Network.Socket
import Prelude.Strings
import System
import Control.Monad.State
import Control.Monad.Trans
import System.Concurrency.Process

import HydraBot.Types
import HydraBot.IRCParsers
import HydraBot.NetworkUtils


test : String -> String -> List String -> StateT Nat IO (List String)
test _ _ ["@inc"] = do
  v <- get
  lift . putStrLn $ "state: " ++ show v
  put (S v)
  pure $ [show v]
test _ _ _ = pure []

basics : List String -> Message -> List Message
basics channels (Msg _ (Right 376) _) = map (msg "JOIN" . pure) channels
basics _ (Msg _ (Left "PING") p) = msgl "PONG" p
basics _ _ = []

||| Message processor out of a pure function
pureProc : (ProcID Message) -> (Message -> List Message) -> Process Message ()
pureProc w f = do
  m <- recv
  traverse (send w) (f m)
  pureProc w f

||| Message processor, IO and State included
sioProc : (ProcID Message) -> (x:s) -> (Message -> StateT s IO (List Message)) -> Process Message ()
sioProc w s f = do
  m <- recv
  (ml, s') <- Lift $ runStateT (f m) s
  traverse (send w) ml
  sioProc w s' f

||| Channel messages processor
sioChannel : (String -> String -> List String -> StateT s IO (List String)) ->
           Message -> StateT s IO (List Message)
sioChannel f (Msg (Just (User u)) (Left "PRIVMSG") (c::rest)) = map (map (cmsg c)) $ f u c rest
sioChannel _ _ = pure []


||| Writer process
||| @s A connected socket
writerProc : (s: Socket) -> Process Message ()
writerProc s = do
  m <- recv
  Lift . putStrLn $ "> " ++ show m
  Lift . sendLine s $ show m
  writerProc s

||| Reader process
||| @pl A list of processes for broadcast
||| @s A connected socket
readerProc : (pl: List (ProcID Message)) -> (s: Socket) -> Process Message ()
readerProc pl s = do
  line <- Lift $ recvTill s "\r\n"
  case line of
    Left err => Lift $ putStrLn $ "Socket error on recv: " ++ show err
    Right str => do
      Lift $ putStr $ "< " ++ str
      broadcast (parse message str)
      readerProc pl s
where
  broadcast : (Either String Message) -> Process Message ()
  broadcast (Left err) = Lift (putStrLn err)
  broadcast (Right msg) = do
    traverse (flip send msg) pl
    return ()


||| Entry point: processes args, connects, runs processes
main : IO ()
main = do
  args <- getArgs
  case args of
    (prog::host::port::n::u::ui::c) => do
      sock <- tcpConnect host (cast port)
      case sock of
        Nothing => putStrLn "Failed to connect"
        Just s => do
          lines <- pure (the (List String) [show $ msg "NICK" [n],
                                            show $ msg "USER" [u, "*", "*", ui]])
          traverse (sendLine s) lines
          wid <- run . create $ writerProc s
          bid <- run . create $ pureProc wid $ basics c
          tid <- run . create $ sioProc wid Z (sioChannel test)
          rid <- run $ create (readerProc [bid, tid] s)
          getLine
          return ()
    _ => putStrLn "Arguments: <ipv4 address> <port> <nick> <user> <user info> [#channel1 #channel2...]"

