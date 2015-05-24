import Network.Socket

tcpConnect : String -> Port -> IO (Maybe Socket)
tcpConnect host port = do
  sock <- socket AF_INET Stream 0
  case sock of
    Left _ => return Nothing
    Right s => do
      conn <- connect s (parseIPv4 host) port
      case conn of
        0 => return $ Just s
        ec => const Nothing <$> (close s *> (putStrLn $ "Connection failed, error: " ++ (show ec)))

recvTill : Socket -> String -> IO (Either SocketError String)
recvTill sock till = recvTill' ""
  where
    recvTill' : String -> IO (Either SocketError String)
    recvTill' prev = do
      r <- recv sock 1
      case r of
        e@(Left err) => return e
        (Right (c, _)) => let s = prev ++ c in
          if isSuffixOf till s
          then return $ Right s
          else recvTill' s

sendLine : Socket -> String -> IO (Either SocketError ByteLength)
sendLine s str = send s (str ++ "\r\n")
