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
import Data.Vect

import HydraBot.Types
import HydraBot.IRCParsers
import HydraBot.IRCProcess
import HydraBot.NetworkUtils
import HydraBot.News

||| Slap people
||| @mn Bot nick, used as a protection against self-slap
||| @u User
||| @c Channel
||| @m Message words
slap : (mn: String) -> (u: String) -> (c: String) ->
     (m: List String) -> StateT (Fin 6) IO (List String)
slap mn u c ["slap", t] = do
  v <- get
  lift . putStrLn $ u ++ " wants to slap " ++ t ++ " on " ++ c
  put $ incr v
  pure $ [action $ wrap (if mn == t then u else t) $ index v slaps]
where
  wrap : String -> (String, String) -> String
  wrap x (y, z) = y ++ " " ++ x ++ z
  incr : Fin (S n) -> Fin (S n)
  incr f = case strengthen (FS f) of
    (Right fs) => fs
    (Left _) => FZ
  slaps : Vect 6 (String, String)
  slaps = [
    ("rewrites", " in whitespace"),
    ("slaps", ""),
    ("touches", " with a hammer"),
    ("grabs", " and runs away"),
    ("rewrites", "'s code in PHP and removes the original"), -- borrowed from fsbot
    ("attacks", "")
    ]
slap _ _ _ _ = pure []


||| Ping and join
||| @c A list of channels to join
||| @m A message to process
basics : (c: List String) -> (m: Message) -> List Message
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
          tid <- sioCommand wid FZ "," (slap n)
          rid <- run $ create (readerProc [bid, tid] s)
          run . create $ news wid c 300 comics
          getLine
          sendLine s . show $ msg "QUIT" ["Time to sleep"]
          close s
    _ => putStrLn "Arguments: <ipv4 address> <port> <nick> <user> <user info> [#channel1 #channel2...]"

