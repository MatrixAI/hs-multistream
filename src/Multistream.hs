{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns      #-}

module Network.Multistream (
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
type Multistream = Text

data MultistreamMessage = MSLs
                        | MSNa
                        | MSSelectedProtocol Multistream
                        | MSProtocolList [Multistream]

data Handler = Handler
    {
        matchingFunc :: Multistream -> Bool,
        hName        :: Multistream,
        handleConn   :: (Connection -> IO ())
    }

data MultistreamMuxer = MultistreamMuxer
    {
        handlers :: [Handler]
    }

msProtocol :: MultistreamMessage
msProtocol = MSSelectedProtocol "/multistream/1.0.0"

getVarInt :: Parser Word32
getVarInt = loop 1 0
  where
    loop !s !n = do
        b <- anyWord8
        let n' = n + s * fromIntegral (b .&. 127)
        if (b .&. 128) == 0
            then return $! n'
            else loop (128*s) n'

parseMultistream :: Parser Multistream
parseMultistream = do
    len <- getVarInt
    protocol <- take len
    string "\n"
    return $ TE.init . TE.decodeUtf8 protocol

parseLs :: Parser MultistreamMessage
parseLs = do
    getVarInt
    string "ls\n"
    return MSLs

parseNa :: Parser MultistreamMessage
parseNa = do
    getVarInt
    string "na\n"
    return MSNa

parseSelectedProtocol :: Parser MultistreamMessage
parseSelectedProtocol = parseMultistream >>= return . MSSelectedProtocol

parseProtocolList :: Parser MultistreamMessage
parseProtocolList = do
    lenLine <- getVarInt
    lenListProtocols <- getVarInt
    nProtocols <- getVarInt
    listProtocols <- count nProtocols parseMultistream
    return $ MSProtocolList listProtocols

putByteStringWith :: ByteString -> Put
putByteStringWithLength bs = do
    serialize (VarInt (BS.length bs))
    putByteString bs

writeMultistream :: OutputStream ByteString -> MultistreamMessage -> IO ()
writeMultistream os (MSProtocolList ps) = do
    let nProtocols = length ps
    let 
    
writeMultistream os ms = do
    let msText = case ms of
                     MSLs                        -> "ls"
                     MSNa                        -> "na"
                     MSSelectedProtocol protocol -> protocol

    TE.encodeUtf8 $ T.snoc msg '\n'
    S.write (Just $ runPutS $ putByteStringWithLength bsMsg) os

addHandler :: Handler -> MultistreamMuxer -> MultistreamMuxer
addHandler h m@(MultistreamMuxer {handlers = old}) = m { handlers = h:old }

negotiate :: Connection -> MultistreamMuxer -> IO ()
negotiate conn@(is, os) mux = do
    header <- parseFromStream parseProtocols is
    r <- parseFromStream msParser is
    case r of
        MSList -> do
            let protos = map (hName) $ handlers mux
            mapM_ (writeMultistream os) protos
            negotiate conn mux

        MSSelect p ->
            case find (matchHandler p) $ handlers mux of
                Nothing -> do
                    writeMultistream os msVersion
                    writeMultistream os "na"
                    negotiate conn mux

                Just h -> do
                    writeMultistream os $ hName h
                    (handleConn h) conn
                  where
                    matchHandler :: Multistream -> Handler -> Bool
                    matchHandler p h = (hName h) == p

handle :: Connection -> MultistreamMuxer -> IO ()
handle conn mux = do
    -- Greeting
    header <- parseFromStream parseProtocols is
    writeMultistream os msProtocol
    if header == msProtocol 
      then
        negotiate conn mux
      else
        putStrLn "Incompatible multistream headers"
