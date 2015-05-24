import Lightyear
import Lightyear.Strings

import HydraBot.Types

numeric : Parser Int
numeric = foldl (\x,y => x * 10 + y) 0 . map (cast . finToNat) <$> ntimes 3 digit

middle : Parser String
middle = pack <$> (some $ satisfy $ not . isSpace)

trailing : Parser String
trailing = pack <$> many (satisfy $ not . flip List.elem ['\r', '\n'])

params : Parser (List String)
params = space *> ((pure <$> (char ':' *> trailing)) <|> ((::) <$> middle <*>| params))

command : Parser (Either String Int)
command = Right <$> numeric <|> Left <$> middle

prefix' : Parser Prefix
prefix' = User <$> (uname <* (char '!' *> middle))
        <|> Server <$> middle
  where
    uname : Parser String
    uname = pack <$> (some $ satisfy (not . flip List.elem ['\r', '\n', ' ', '!']))

message : Parser Message
message = Msg
        <$> ((char ':' *> Just <$> prefix' <* space) <|> (pure Nothing))
        <*> command
        <*> params
        <*  string "\r\n"
