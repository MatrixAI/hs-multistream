module Main where

import Multistream
import System.IO.Streams as S

spongebobHandler = Handler "/spongebob" (\is -> putStrLn "where's my pineapple")
garyHandler = Handler "/gary" (\is -> putStrLn "...")

m1 = MultistreamMuxer "/multistream-select/1.0" [spongebobHandler, garyHandler]

main :: IO ()
main = do
    test <- S.fromList ["/multistream-select/1.0", 
                        "/multistream-select/1.0", "ls", 
                        "/multistream-select/1.0", "/squidward",
                        "/multistream-select/1.0", "/spongebob"]
    handle test m1
