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

||| Slaps
slaps : List (String, String)
slaps = [
  ("rewrites", " in whitespace"),
  ("slaps", ""),
  ("grabs", " and runs away"),
  ("rewrites", "'s code in PHP and removes the original"), -- borrowed from fsbot
  ("attacks", ""),
  ("hugs", " instead"),
  ("beta-reduces", ""),
  ("shows that", " is homotopy equivalent to a point")
  ]


||| Slap people
||| @mn Bot nick, used as a protection against self-slap
||| @u User
||| @c Channel
||| @m Message words
slap : (mn: String) -> (u: String) -> (c: String) ->
     (m: List String) -> StateT (Fin (length slaps)) IO (List String)
slap mn u c ["slap", t] = do
  v <- get
  lift . putStrLn $ u ++ " wants to slap " ++ t ++ " on " ++ c
  put $ incr v
  pure $ [action $ wrap (if mn == t then u else t) $ index v (fromList slaps)]
where
  wrap : String -> (String, String) -> String
  wrap x (y, z) = y ++ " " ++ x ++ z
  incr : Fin (S n) -> Fin (S n)
  incr f = case strengthen (FS f) of
    (Right fs) => fs
    (Left _) => FZ
slap _ _ _ _ = pure []


||| Ping and join
||| @c A list of channels to join
||| @m A message to process
basics : (c: List String) -> (m: Message) -> List Message
basics channels (Msg _ (Right 376) _) = map (msg "JOIN" . pure) channels
basics _ (Msg _ (Left "PING") p) = msgl "PONG" p
basics _ _ = []


||| Common things
common : (u: String) -> (c: String) -> (m: List String) -> StateT () IO (List String)
common u c ["anyone"] = pure . pure $ -- borrowed from fsbot
  "Just ask your question. It's the best way to know if anyone can help."
common _ _ _ = pure []


||| Knowledge base
kb : (u: String) -> (c: String) -> (m: List String) ->
   StateT (List (String, String)) IO (List String)
kb u c (t::"is"::d) = do
  s <- get
  put $ List.(::) (t, unwords d) s
  pure . pure $ u ++ ": added a definition for " ++ t
kb u c ["forget","about",t] = do
  s <- get
  case List.lookup t s of
    Nothing => pure . pure $ u ++ ": I don't know anything about " ++ t ++ " anyway"
    Just _ => do
      put $ List.filter (not . (== t) . fst) s
      pure . pure $ u ++ ": forgot successfully"
kb u c ("forget"::"about"::t::"being"::d) = do
  s <- get
  if List.elem (t, unwords d) s
    then do
      put $ List.filter (not . (== (t, unwords d))) s
      pure . pure $ u ++ ": forgot successfully"
    else pure . pure $ u ++ "I don't even know about " ++ t ++ " being " ++ unwords d
kb u c [t] = do
  s <- get
  case List.filter ((== t) . fst) s of
    [] => pure []
    [x] => pure . pure $ t ++ " is " ++ snd x
    xs => pure $ (u ++ ": " ++ t ++ " could refer to any of these things:") ::
      map ((++) (u ++ ": - ") . snd) xs
kb _ _ _ = pure []


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
          traverse {t=List} (sendLine s) [show $ msg "NICK" [n],
                                          show $ msg "USER" [u, "*", "*", ui]]
          wid <- run . create $ writerProc s
          bid <- run . create $ pureProc wid $ basics c
          tid <- sioCommand wid FZ "," (slap n)
          cid <- sioCommand wid () "," common
          kid <- sioCommand wid List.Nil "," kb
          rid <- run . create $ readerProc [bid, tid, cid, kid] s
          nid <- run . create $ news wid c 300 comics
          getLine
          sendLine s . show $ msg "QUIT" ["Time to sleep"]
          close s
    _ => putStrLn "Arguments: <ipv4 address> <port> <nick> <user> <user info> [#channel1 #channel2...]"

