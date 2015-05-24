data Prefix = User String | Server String

record Message where
  constructor Msg
  msg_prefix : Maybe Prefix
  msg_command : Either String Int
  msg_params : List String


-- utils

join : String -> List String -> String
join sep ls = pack $ intercalate (unpack sep) (map unpack ls)


instance Show Message where
  show (Msg pref cmd par) = pref' ++ cmd' ++ " " ++ join " " (map p par)
  where
    pref' = case pref of
      Nothing => ""
      Just (User s) => ":" ++ s ++ " "
      Just (Server s) => ":" ++ s ++ " "
    cmd' = case cmd of
      Left s => s
      Right n => show n
    p s = if length (words s) > 1 then ":" ++ s else s


-- simplified message constructors

msg : String -> List String -> Message
msg s l = Msg Nothing (Left s) l

msgl : String -> List String -> List Message
msgl s l = pure $ msg s l

cmsg : String -> String -> Message
cmsg c m = msg "PRIVMSG" [c, m]

cmsgl : String -> String -> List Message
cmsgl c m = pure $ cmsg c m
