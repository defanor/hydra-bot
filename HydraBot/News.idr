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
||| @d Delay between checks, in seconds
||| @l A list of news loaders
||| @o A list of previous versions
news' : (w: ProcID Message) -> (c: List String) -> (d: Int) ->
      (l: Vect n (String, IO (Maybe String))) -> (o: Vect n (Maybe String)) ->
      Process Message ()
news' writer channels delay loaders old = do
  Lift $ usleep $ delay * 1000000
  new <- traverse check $ zip loaders old
  news' writer channels delay loaders new
where
  check : ((String, IO (Maybe String)), Maybe String) -> Process Message (Maybe String)
  check ((name, load), o) = do
    n <- Lift load
    case (n, n == o) of
      (Just n', False) => do
        if (o /= Nothing) -- don't write right after [re]start
        then traverse (\x => send writer (cmsg x (name ++ ": " ++ n'))) channels
        else pure []
        pure (Just n')
      (Nothing, False) => do
        Lift . putStrLn $ "Failed to retrieve: " ++ name
        pure o
      (_, _) => pure o

||| Check news/updates
||| @w A writer process id
||| @c A list of channels
||| @d Delay between checks, in seconds
||| @l A list of news loaders
news : (w: ProcID Message) -> (c: List String) -> (d: Int) ->
     (l: Vect n (String, IO (Maybe String))) -> Process Message ()
news w c d l = news' w c d l (map (const Nothing) l)



skipTill : String -> Parser String
skipTill s = string s <|>| ((satisfy $ const True) *> (skipTill s))

tillChar : Char -> Parser String
tillChar c = pack <$> many (satisfy $ (/= c))

||| XKCD scraper
xkcd : IO (Maybe String)
xkcd = scrape "23.235.37.67" 80 req "</feed>" url
where
  req = "GET /atom.xml HTTP/1.0\r\nHost: xkcd.com\r\n"
  url : Parser String
  url = do
    skipTill "<entry><title>"
    title <- tillChar '<'
    skipTill "<link href=\""
    url <- tillChar '"'
    pure $ url ++ " (" ++ title ++ ")"

||| Invisible Bread scraper
IB : IO (Maybe String)
IB = scrape "104.28.24.25" 80 req "<link rel=\"shortcut" url
where
  req = "GET / HTTP/1.0\r\nHost: invisiblebread.com\r\n"
  url : Parser String
  url = do
    skipTill "<meta property=\"og:url\" content=\""
    url <- tillChar '"'
    pure url


comics : Vect 2 (String, IO (Maybe String))
comics = [("XKCD", xkcd), ("Invisible Bread", IB)]
