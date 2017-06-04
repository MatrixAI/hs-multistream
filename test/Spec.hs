{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.Multistream        
import           Network.Multistream.Muxer
import qualified System.IO.Streams         as S
import           System.IO.Streams.Handle  (stdout)
import           Data.Text                 (Text)
import qualified Data.Text.Encoding        as TE

h1 = Handler (encodeText "/spongebob") $
    (\(is, os) -> S.write (Just $ TE.encodeUtf8 "where's my pineapple") os 
    >> S.write Nothing os)

h2 = Handler (encodeText "/gary") $ 
    (\(is, os) -> S.write (Just $ TE.encodeUtf8 "...") os 
    >> S.write Nothing os)

m1 = MultistreamMuxer [h1, h2]

testNegotiate = ["/multistream-select/1.0", 
                 "/multistream-select/1.0", "ls", 
                 "/multistream-select/1.0", "/squidward",
                 "/multistream-select/1.0", "/spongebob"]

main :: IO ()
main = do
    test <- S.fromList $ map TE.encodeUtf8 testNegotiate
    handle (test, stdout) m1
