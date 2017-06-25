{-# LANGUAGE OverloadedStrings #-}

module Network.Multistream.Message (
    MultistreamMessage(..),
    ls,
    newProtocol,
    protocolVersion,
    parseLs,
    parseNa,
    parseSelectedProtocol,
    parseMultistreamLineList,
    putMultistreamMessage
) where


import           Data.Attoparsec.ByteString (Parser, count)
import           Data.Bytes.Put             (runPutS)
import           Data.Bytes.Serial          (serialize, serializeWith)
import           Data.Bytes.VarInt          (VarInt (..))
import qualified Data.ByteString            as BS
import           Data.Serialize.Put         (Put)
import           Data.Text                  (Text)

import           Network.Multistream        (Multistream, encodeText,
                                             parseMatchText, parseMultistreamLine,
                                             parseVarInt, putMultistreamLine)
import qualified Network.Multistream        as M

data MultistreamMessage = MSLs
                        | MSNa
                        | MSSelectedProtocol Multistream
                        | MSProtocolList [Multistream]
                        deriving (Eq)

newProtocol :: Text -> MultistreamMessage
newProtocol t = MSSelectedProtocol $ encodeText t

ls :: MultistreamMessage
ls = MSLs

protocolVersion :: MultistreamMessage
protocolVersion = newProtocol "/multistream/1.0.0"

parseLs :: Parser MultistreamMessage
parseLs = do
    parseMatchText "ls"
    return MSLs

parseNa :: Parser MultistreamMessage
parseNa = do
    parseMatchText "na"
    return MSNa

parseSelectedProtocol :: Parser MultistreamMessage
parseSelectedProtocol = parseMultistreamLine >>= return . MSSelectedProtocol

parseMultistreamLineList :: Parser MultistreamMessage
parseMultistreamLineList = do
    lenLine <- parseVarInt
    lenListProtocols <- parseVarInt
    nProtocols <- parseVarInt
    listProtocols <- count nProtocols parseMultistreamLine
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

