-- Local Variables:
-- idris-packages: ("lightyear" "contrib")
-- End:

-- Quick and dirty bot

import Lightyear.Strings
import Network.Socket
import Prelude.Strings
import System
import Control.Monad.State
import Control.Monad.Trans

import MessyBot.Types
import MessyBot.IRCParsers
import MessyBot.NetworkUtils


Bot : Type -> Type
Bot = StateT BotState IO

action : String -> Bot String
action s = pure $ "\001ACTION " ++ s ++ "\001"

actionl : List String -> Bot (List String)
actionl s = traverse action s


-- Bot behaviour (i.e. commands) is defined below

||| A command (prefixed channel message) processor.
||| Returns a message to send back to that channel.
||| @u User nick
||| @m Message words
commandMessage : (u: String) -> (m: List String) -> Bot String
commandMessage u ["inc"] = do
  bs <- get
  put $ record { counter = counter bs + 1 } bs
  pure $ u ++ ": " ++ show (counter bs)
commandMessage _ ["slap", nick] = action $ "slaps " ++ nick
commandMessage _ _ = pure ""

||| IRC actions processor.
||| Returns a list of messages to react on an action.
||| @u User nick
||| @a Action words
actionMessages : (u: String) -> (a: List String) -> Bot (List String)
actionMessages u ["slaps", u2] = actionl ["slaps " ++ u]
actionMessages u ["hugs", u2] = actionl ["hugs " ++ u]
actionMessages _ _ = pure []


-- The rest is mostly bootstrap


||| Channel messages processor, returns a list of messages to send
||| @u User nick
||| @c Channel name
||| @m A received message
chanMessages : (u: String) -> (c: String) -> (m: String) -> Bot (List Message)
chanMessages u c m = case (strHead m) of
  ',' => do
    r <- commandMessage u (words $ strTail m)
    case r of
      "" => pure []
      msg => pure $ cmsgl c msg
  _ => case (isPrefixOf "\001ACTION " m && isSuffixOf "\001" m) of
    False => pure []
    True => case (init' $ drop 8 $ unpack m) of
      Nothing => pure []
      Just l => do
        msgs <- actionMessages u $ words $ pack l
        case msgs of
          [] => pure []
          l => pure $ map (cmsg c) msgs

||| Parsed IRC messages processor, returns a list of messages to send.
botMessages : Message -> Bot (List Message)
botMessages (Msg _ (Right 376) _) = do
  bs <- get
  pure $ map (msg "JOIN" . pure) $ channels bs
botMessages (Msg _ (Left "PING") p) = pure $ msgl "PONG" p
botMessages m@(Msg (Just (User u)) (Left "PRIVMSG") [c,msg]) = do
  bs <- get
  if c `elem` (channels bs)
  then chanMessages u c msg
  else return []
botMessages m = pure []

||| Raw IRC messages processor: simply parses them and passes to
||| botMessages. Returns a list of messages to send.
||| @s Raw message.
botLines : (s: String) -> Bot (List String)
botLines s = case (parse message s) of
  Left err => do
    lift $ putStrLn err
    return []
  Right msg => map (map show) (botMessages msg)

||| Main loop
||| @s A connected socket
botLoop : (s: Socket) -> Bot ()
botLoop s = do
  line <- lift $ recvTill s "\r\n"
  case line of
    Left err => lift $ putStrLn $ "Socket error on recv: " ++ show err
    Right str => do
      lift $ putStr $ "< " ++ str
      lines <- botLines str
      r <- lift $ traverse (sendLine s) lines
      -- might be worthwhile to check results here
      botLoop s

||| Entry point: processes args, connects, runs botLoop
main : IO ()
main = do
  args <- getArgs
  case args of
    (prog::host::port::n::u::ui::c) => do
      sock <- tcpConnect host (cast port)
      case sock of
        Nothing => putStrLn "Failed to connect"
        Just s => do
          lines <- pure (the (List String) [show $ msg "NICK" [n], show $ msg "USER" [u, "*", "*", ui]])
          traverse (sendLine s) lines
          runStateT (botLoop s) $ record { channels = c } defaultState
          return ()
    _ => putStrLn "Arguments: <server> <port> <nick> <user> <user info> [#channel1 #channel2...]"


