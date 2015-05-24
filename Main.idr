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
import HydraBot.IRCProcess
import HydraBot.NetworkUtils


test : String -> String -> List String -> StateT Nat IO (List String)
test _ _ ["inc"] = do
  v <- get
  lift . putStrLn $ "state: " ++ show v
  put (S v)
  pure $ [show v]
test _ _ _ = pure []

basics : List String -> Message -> List Message
basics channels (Msg _ (Right 376) _) = map (msg "JOIN" . pure) channels
basics _ (Msg _ (Left "PING") p) = msgl "PONG" p
basics _ _ = []


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
          tid <- sioCommand wid Z "," test
          rid <- run $ create (readerProc [bid, tid] s)
          getLine
          return ()
    _ => putStrLn "Arguments: <ipv4 address> <port> <nick> <user> <user info> [#channel1 #channel2...]"

