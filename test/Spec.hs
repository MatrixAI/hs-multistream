{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Bytes.Put              (runPutS)

import           Network.Multistream         (encodeText)
import qualified Network.Multistream.Message as MS
import           Network.Multistream.Muxer   (Handler(..), MultistreamMuxer(..))
import qualified Network.Multistream.Muxer   as MX

import qualified System.IO.Streams           as S
import           System.IO.Streams.Handle    (stderr)
import           Data.Text                   (Text)
import qualified Data.Text.Encoding          as TE

h1 = Handler (encodeText "/spongebob") $
    (\(is, os) -> S.write (Just $ TE.encodeUtf8 "where's my pineapple") os 
    >> S.write Nothing os)

h2 = Handler (encodeText "/gary") $ 
    (\(is, os) -> S.write (Just $ TE.encodeUtf8 "...") os 
    >> S.write Nothing os)

m1 = MultistreamMuxer [h1, h2]

testNegotiate = [MS.protocolVersion,
                 MS.protocolVersion, MS.ls,
                 MS.protocolVersion, MS.newProtocol "/squidward",
                 MS.protocolVersion, MS.newProtocol "/spongebob"]

main :: IO ()
main = do
    test <- S.fromList $ map (runPutS . MS.putMultistreamMessage) testNegotiate
    MX.handle (test, stderr) m1
