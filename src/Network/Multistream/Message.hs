{-# LANGUAGE OverloadedStrings #-}

module Network.Multistream.Message (
    MultistreamMessage(..),
    protocolVersion,
    parseLs,
    parseNa,
    parseSelectedProtocol,
    parseProtocolList,
    putMultistreamMessage
) where


import           Data.Attoparsec.ByteString (Parser, count, string)
import           Data.Bytes.Put             (runPutS)
import           Data.Bytes.Serial          (serialize, serializeWith)
import           Data.Bytes.VarInt          (VarInt (..))
import qualified Data.ByteString            as BS
import           Data.Serialize.Put         (Put)

import           Network.Multistream        (Multistream, parseMatchText,
                                             parseProtocol, parseVarInt,
                                             putMultistreamLine)
import qualified Network.Multistream        as M

data MultistreamMessage = MSLs
                        | MSNa
                        | MSSelectedProtocol Multistream
                        | MSProtocolList [Multistream]
                        deriving (Eq)

protocolVersion :: MultistreamMessage
protocolVersion = MSSelectedProtocol $ M.encodeText "/multistream/1.0.0"

parseLs :: Parser MultistreamMessage
parseLs = do
    parseMatchText "ls"
    return MSLs

parseNa :: Parser MultistreamMessage
parseNa = do
    parseMatchText "na"
    return MSNa

parseSelectedProtocol :: Parser MultistreamMessage
parseSelectedProtocol = parseProtocol >>= return . MSSelectedProtocol

parseProtocolList :: Parser MultistreamMessage
parseProtocolList = do
    lenLine <- parseVarInt
    lenListProtocols <- parseVarInt
    nProtocols <- parseVarInt
    listProtocols <- count nProtocols parseProtocol
    return $ MSProtocolList listProtocols

putMultistreamMessage :: MultistreamMessage -> Put
putMultistreamMessage (MSProtocolList xs) = do
    putMultistreamLine (M.encodeByteString $ runPutS header)
    putProtocolList
      where
        putProtocolList :: Put
        putProtocolList = serializeWith putMultistreamLine xs

        header :: Put
        header = do
            let b = runPutS putProtocolList
            serialize $ VarInt (BS.length b)
            serialize $ VarInt (length xs)

putMultistreamMessage m = do
    case m of
        MSLs                  -> putMultistreamLine (M.encodeText "ls")
        MSNa                  -> putMultistreamLine (M.encodeText "na")
        MSSelectedProtocol ms -> putMultistreamLine ms
