{-# LANGUAGE OverloadedStrings #-}

module Network.Multistream.Muxer (
    Handler(..),
    MultistreamMuxer(..),
    addHandler,
    negotiateHandler,
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

newMultistreamMuxer :: MultistreamMuxer
newMultistreamMuxer = MultistreamMuxer []

addHandler :: Handler -> MultistreamMuxer -> MultistreamMuxer
addHandler h m@(MultistreamMuxer {handlers = old}) = m { handlers = h:old }

negotiateHandler :: Connection -> MultistreamMuxer -> IO Handler
negotiateHandler conn@(is, os) mux = do
    header <- parseFromStream MS.parseSelectedProtocol is
    r <- parseFromStream (choice [MS.parseLs, MS.parseSelectedProtocol]) is
    case r of
      MSLs -> do
        let protos = map (hName) $ handlers mux
        writeMultistream os (MSProtocolList protos)
        negotiateHandler conn mux

      MSSelectedProtocol p ->
          case find (matchHandler p) $ handlers mux of
            Nothing -> do
                writeMultistream os MS.protocolVersion
                writeMultistream os MSNa
                negotiateHandler conn mux

            Just h -> do
                putStrLn "handler found"
                putStrLn "replying with handler name"
                writeMultistream os $ MSSelectedProtocol $ hName h
                return h
            where
                matchHandler :: Multistream -> Handler -> Bool
                matchHandler p h = (hName h) == p

handle :: Connection -> MultistreamMuxer -> IO ()
handle conn@(is, os) mux = do
    handshake conn
    putStrLn "matching protocol versions... starting negotiation"
    handler <- negotiateHandler conn mux
    putStrLn "negotiation successful... running handler"
    handleConn handler $ conn

handshake :: Connection -> IO ()
handshake conn@(is,os) = do
    header <- parseFromStream MS.parseSelectedProtocol is 
    if header == MS.protocolVersion
      then do
        writeMultistream os MS.protocolVersion
      else do
        return ()

trySelect :: Connection -> MultistreamMessage -> IO ()
trySelect conn@(is, os) proto = do
    writeMultistream os proto
    reply <- parseFromStream (choice [MS.parseNa, MS.parseSelectedProtocol]) is
    case reply of
        MSNa -> return ()
        MSSelectedProtocol p -> return ()

selectProtocolOrFail :: Connection -> MultistreamMessage -> IO ()
selectProtocolOrFail conn m = do
    handshake conn
    trySelect conn m

writeMultistream :: OutputStream ByteString -> MultistreamMessage -> IO ()
writeMultistream os ms = do
    S.write (Just $ runPutS $ MS.putMultistreamMessage ms) os
