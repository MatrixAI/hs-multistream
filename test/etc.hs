sr1 = ["/http/multiproto.io/multistream-select/1.0"]

s2 = ["/http/multiproto.io/multistream-select/1.0", "ls"]

r2 = ["/http/google.com/spdy/3",
      "/http/w3c.org/http/1.1",
      "/http/w3c.org/http/2",
      "/http/bittorrent.org/1.2",
      "/http/git-scm.org/1.2",
      "/http/ipfs.io/exchange/bitswap/1",
      "/http/ipfs.io/routing/dht/2.0.2",
      "/http/ipfs.io/network/relay/0.5.2"]

s3 = ["/http/multiproto.io/multistream-select/1.0", "ls",
      "/http/w3id.org/http/1.1", "GET / HTTP/1.1", ""]

r3 = ["/http/w3id.org/http/1.1",
      "HTTP/1.1 200 OK", 
      "Content-Type: text/html; charset=UTF-8",
      "Content-Length: 12",
      "Hello World"]

data Peer a = PInt (S.InputStream Int) | PString (S.InputStream String) 

printMirrored :: Peer (S.InputStream String) -> IO ()
printMirrored p@(PString is) = do 
    case S.read is of
       Just str -> putStrLn str >> printMirrored p
       Nothing -> return ()


printAdded :: Peer (S.InputStream Int) -> IO ()
printAdded (PInt is) = do
    x <- S.read is
    y <- S.read is
    putStrLn (show (x + y))


