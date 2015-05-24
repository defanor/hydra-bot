import Lightyear.Strings
import Network.Socket
import System
import Control.Monad.State
import Control.Monad.Trans
import System.Concurrency.Process

import HydraBot.Types
import HydraBot.IRCParsers
import HydraBot.NetworkUtils


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
sioChannel : (String -> String -> String -> StateT s IO (List String)) ->
           Message -> StateT s IO (List Message)
sioChannel f (Msg (Just (User u)) (Left "PRIVMSG") [c,msg]) = map (map (cmsg c)) $ f u c msg
sioChannel _ _ = pure []

||| Run sioChannel for prefixed commands
||| @w Writer process
||| @x Initial state
||| @p Command prefix
||| @f A function to wrap
sioCommand : (w: ProcID Message) -> (x:s) -> (p: String) ->
           (f: String -> String -> List String -> StateT s IO (List String)) ->
           IO (ProcID Message)
sioCommand w st p f = run . create $ sioProc w st (sioChannel f')
where
  f' : String -> String -> String -> StateT s IO (List String)
  f' u c m = if isPrefixOf p m
             then f u c (words $ pack $ unpack m \\ unpack p)
             else pure []


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
