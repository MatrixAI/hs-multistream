{-# LANGUAGE OverloadedStrings #-}

module Network.Multistream.Muxer (
    Handler(..),
    MultistreamMuxer(..),
    addHandler,
    handle
) where

import           Data.Attoparsec.ByteString   (Parser, choice)

import           Data.ByteString              (ByteString)

import           Data.Bytes.Put               (runPutS)

import           Data.List                    (find)

import           Network.Multistream          (Multistream)
import           Network.Multistream.Message  (MultistreamMessage (..))
import qualified Network.Multistream.Message  as MS

import           System.IO.Streams            (InputStream, OutputStream)
import qualified System.IO.Streams            as S
import           System.IO.Streams.Attoparsec (parseFromStream)

type Connection = (InputStream ByteString, OutputStream ByteString)

data MultistreamMuxer = MultistreamMuxer
    {
        handlers :: [Handler]
    }

data Handler = Handler
    {
        hName      :: Multistream,
        handleConn :: (Connection -> IO ())
    }

addHandler :: Handler -> MultistreamMuxer -> MultistreamMuxer
addHandler h m@(MultistreamMuxer {handlers = old}) = m { handlers = h:old }

writeMultistream :: OutputStream ByteString -> MultistreamMessage -> IO ()
writeMultistream os ms = do
    S.write (Just $ runPutS $ MS.putMultistreamMessage ms) os

negotiate :: Connection -> MultistreamMuxer -> IO ()
negotiate conn@(is, os) mux = do
    header <- parseFromStream MS.parseSelectedProtocol is
    r <- parseFromStream (choice [MS.parseLs, MS.parseSelectedProtocol]) is
    case r of
      MSLs -> do
        let protos = map (hName) $ handlers mux
        writeMultistream os (MSProtocolList protos)
        negotiate conn mux

      MSSelectedProtocol p ->
          case find (matchHandler p) $ handlers mux of
            Nothing -> do
                writeMultistream os MS.protocolVersion
                writeMultistream os MSNa
                negotiate conn mux

            Just h -> do
                writeMultistream os $ MSSelectedProtocol $ hName h
                (handleConn h) conn
            where
                matchHandler :: Multistream -> Handler -> Bool
                matchHandler p h = (hName h) == p

handle :: Connection -> MultistreamMuxer -> IO ()
handle conn@(is, os) mux = do
    -- Greeting
    putStrLn "parsing header"
    header <- parseFromStream MS.parseSelectedProtocol is
    putStrLn "replying with protocol version"
    writeMultistream os MS.protocolVersion
    if header == MS.protocolVersion
      then do
        putStrLn "matching protocol versions... starting negotiation"
        negotiate conn mux
      else do
        putStrLn "couldn't match protocol versions... terminating"
