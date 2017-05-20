{-# LANGUAGE OverloadedStrings #-}

module Multistream (
    Handler(..),
    MultistreamMuxer(..),
    addHandler,
    handle
) where

import           Data.Attoparsec.ByteString   (Parser)
import           Data.ByteString              (ByteString)
import           Data.ByteString.Builder      (Builder, charUtf8)
import           Data.List                    (find)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE

import           System.IO.Streams            (InputStream, OutputStream)
import qualified System.IO.Streams            as S
import           System.IO.Streams.Attoparsec (parseFromStream)

type Connection = (InputStream ByteString, OutputStream ByteString)

data Handler = Handler
    {
        matchingFunc :: ByteString -> Bool,
        name         :: Text,
        handleConn   :: (Connection -> IO ())
    }

data MultistreamMuxer = MultistreamMuxer
    {
        handlers :: [Handler]
    }


msVersion :: Text
msVersion = ("/multistream/1.0.0" :: Text)

-- todo: implement this
msParser :: Parser Text
msParser = undefined
-- todo: need to swap out normal strings for multistream serialised strings
-- with varints at front

msSerialize :: Text -> ByteString
msSerialize s = (TE.encodeUtf8 s) `snoc` charUtf8 '\n'

addHandler :: Handler -> MultistreamMuxer -> MultistreamMuxer
addHandler h m@(MultistreamMuxer {handlers = old}) = m { handlers = h:old }

matchingHeader :: Connection -> MultistreamMuxer -> IO Bool
matchingHeader (is, os) mux = do
    header <- parseFromStream msParser is
    if header == msVersion
        then do
            S.write (Just $ TE.encodeUtf8 header) os
            return True
        else do
            -- swap out these for multistream serialised strings
            S.write (Just $ TE.encodeUtf8 "non-matching multistream version") os
            S.write Nothing os
            return False

handle :: Connection -> MultistreamMuxer -> IO ()
handle conn mux = do
    matchedHeader <- matchingHeader conn mux
    if matchedHeader
        then do
            negotiate conn mux
        else
            return ()

      where
        negotiate :: Connection -> MultistreamMuxer -> IO ()
        negotiate conn@(is, os) mux = do
            matchedHeader <- matchingHeader conn mux
            if not matchedHeader
                then return ()
            else do
                protocol <- parseFromStream msParser is
                case protocol of
                    -- Need some error catching logic here to handle
                    -- if the stream was closed unexpectedly
                    Nothing -> do
                        -- swap out these for multistream serialised strings
                        S.write (Just "stream closed unexpectedly") os

                    Just "ls" -> do
                        let protos = map (Just . handlerProtocol) $ handlers mux
                        mapM_ (flip S.write os) protos
                        negotiate conn mux

                    Just p ->
                        case find (matchHandler p) $ handlers mux of
                            Nothing -> do
                                S.write (Just msVersion) os
                                S.write (Just "na") os
                                negotiate conn mux

                            Just h -> do
                                S.write (Just $ handlerProtocol h) os
                                (handleConn h) conn

                            where
                                matchHandler :: String -> Handler -> Bool
                                matchHandler p h = (handlerProtocol h) == p
