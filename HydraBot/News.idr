import Lightyear
import Lightyear.Strings
import Network.Socket
import Prelude.Strings
import System
import System.Concurrency.Process
import Data.Vect

import HydraBot.Types
import HydraBot.NetworkUtils


||| Check news/updates
||| @w A writer process id
||| @c A list of channels
||| @l A list of news loaders
||| @o A list of previous versions
news' : (w: ProcID Message) -> (c: List String) ->
      (l: Vect n (String, IO (Maybe String))) -> (o: Vect n (Maybe String)) ->
      Process Message ()
news' w channels loaders old = do
  Lift $ usleep $ 60 * 1000 * 1000
  new <- traverse check $ zip loaders old
  news' w channels loaders new
where
  check : ((String, IO (Maybe String)), Maybe String) -> Process Message (Maybe String)
  check ((name, load), o) = do
    n <- Lift load
    case (n, n == o) of
      (Just n', False) => do
        traverse (\x => Process.send w (cmsg x (name ++ ": " ++ n'))) channels
        pure (Just n')
      (Nothing, False) => do
        Lift . putStrLn $ "Failed to retrieve: " ++ name
        pure o
      (_, _) => pure o

||| Check news/updates
||| @w A writer process id
||| @c A list of channels
||| @l A list of news loaders
news : (w: ProcID Message) -> (c: List String) ->
     (l: Vect 1 (String, IO (Maybe String))) -> Process Message ()
news w c l = news' w c l (map (const Nothing) l)



skipTill : String -> Parser String
skipTill s = string s <|>| ((satisfy $ const True) *> (skipTill s))

||| XKCD loader
xkcd : IO (Maybe String)
xkcd = do
  ms <- tcpConnect "23.235.37.67" 80
  case ms of
    Nothing => pure Nothing
    Just s => do
      sendLine s "GET /atom.xml HTTP/1.0\r\nHost: xkcd.com\r\n"
      d <- recvTill s "</feed>"
      case d of
        Left err => pure Nothing
        Right str => processBody (parse url str)
where
  processBody : Either String String -> IO (Maybe String)
  processBody (Left err) = pure Nothing
  processBody (Right b) = do
    putStrLn b
    pure $ Just b
  url : Parser String
  url = do
    skipTill "<entry><title>"
    title <- pack <$> many (satisfy $ (/= '<'))
    skipTill "<link href=\""
    url <- pack <$> many (satisfy $ (/= '"'))
    pure $ url ++ " (" ++ title ++ ")"

comics : Vect 1 (String, IO (Maybe String))
comics = [("XKCD", xkcd)]
