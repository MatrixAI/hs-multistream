{-# LANGUAGE OverloadedStrings #-}

module Multistream (
    Handler(..),
    MultistreamMuxer(..),
    addHandler,
    handle
) where

import           Data.Attoparsec.ByteString   (Parser, maybeResult)

import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS

import           Data.Bytes.Put               (putByteString, runPutS)
import           Data.Bytes.Serial            (serialize)
import           Data.Bytes.VarInt            (VarInt (..))
import           Data.Serialize.Put           (Put)

import           Data.List                    (find)

import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE

import           Data.Word                    (Word8)

import           System.IO.Streams            (InputStream, OutputStream)
import qualified System.IO.Streams            as S
import           System.IO.Streams.Attoparsec (parseFromStream)

type Connection = (InputStream ByteString, OutputStream ByteString)

data Handler = Handler
    {
        matchingFunc :: ByteString -> Bool,
        hName        :: Text,
        handleConn   :: (Connection -> IO ())
    }

data MultistreamMuxer = MultistreamMuxer
    {
        handlers :: [Handler]
    }


msVersion :: Text
msVersion = "/multistream/1.0.0"

-- todo: implement this read messages
msParser :: Parser Text
msParser = undefined

delimitedWrite :: OutputStream ByteString -> Text -> IO ()
delimitedWrite os msg = do
    let bsMsg = TE.encodeUtf8 $ T.snoc msg '\n'
    S.write (Just $ runPutS $ putMsgWithLen bsMsg) os
    where
        putMsgWithLen :: ByteString -> Put
        putMsgWithLen bs = do
            serialize (VarInt (BS.length bs))
            putByteString bs

addHandler :: Handler -> MultistreamMuxer -> MultistreamMuxer
addHandler h m@(MultistreamMuxer {handlers = old}) = m { handlers = h:old }

matchingHeader :: Connection -> MultistreamMuxer -> IO Bool
matchingHeader (is, os) mux = do
    header <- parseFromStream msParser is
    if header == msVersion
        then do
            delimitedWrite os header
            return True
        else do
            return False

negotiate :: Connection -> MultistreamMuxer -> IO ()
negotiate conn@(is, os) mux = do
    matchedHeader <- matchingHeader conn mux
    if not matchedHeader
        then return ()
    else do
        r <- parseFromStream msParser is
        case r of
            "ls" -> do
                let protos = map (hName) $ handlers mux
                mapM_ (delimitedWrite os) protos
                negotiate conn mux

            p ->
                case find (matchHandler p) $ handlers mux of
                    Nothing -> do
                        delimitedWrite os msVersion
                        delimitedWrite os "na"
                        negotiate conn mux

                    Just h -> do
                        delimitedWrite os $ hName h
                        (handleConn h) conn

                    where
                        matchHandler :: Text -> Handler -> Bool
                        matchHandler p h = (hName h) == p

handle :: Connection -> MultistreamMuxer -> IO ()
handle conn mux = do
    matchedHeader <- matchingHeader conn mux
    if matchedHeader
        then do
            negotiate conn mux
        else
            return ()

