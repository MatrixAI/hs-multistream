{-# LANGUAGE OverloadedStrings #-}

module Network.Multistream (
    Multistream,
    encodeByteString,
    encodeText,
    decodeMultistream,
    multistreamLength,
    parseVarInt,
    parseMatchText,
    parseMultistreamLine,
    putMultistreamLine
) where

import           Data.Attoparsec.ByteString (Parser, anyWord8, string)
import qualified Data.Attoparsec.ByteString as PS
import           Data.Bits                  (clearBit, shiftL, testBit, (.|.))
import           Data.Bytes.Put             (putByteString)
import           Data.Bytes.Serial          (serialize)
import           Data.Bytes.VarInt          (VarInt (..))
import           Data.ByteString            (ByteString)
import           Data.Serialize.Put         (Put)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)

newtype Multistream = Multistream Text deriving (Eq)

encodeByteString :: ByteString -> Multistream
encodeByteString bs = Multistream $ decodeUtf8 bs

encodeText :: Text -> Multistream
encodeText t = Multistream t

decodeMultistream :: Multistream -> Text
decodeMultistream (Multistream m) = m

multistreamLength :: Multistream -> Int
multistreamLength (Multistream m)= T.length m

parseVarInt :: Parser Int
parseVarInt = do
  n <- anyWord8
  if testBit n 7
    then do
      m <- parseVarInt
      return $ shiftL m 7 .|. clearBit (fromIntegral n) 7
    else do
      return $ fromIntegral n

parseMatchText :: Text -> Parser ()
parseMatchText s = do
    parseVarInt
    string $ encodeUtf8 s
    string $ encodeUtf8 "\n"
    return ()

--TODO: ensure that the protocol has some "/a/b" type format
parseMultistreamLine :: Parser Multistream
parseMultistreamLine = do
    len <- parseVarInt
    protocol <- PS.take (len - 1)
    string $ encodeUtf8 "\n"
    return $ Multistream $ decodeUtf8 protocol

putMultistreamLine :: Multistream -> Put
putMultistreamLine (Multistream m) = do
    serialize (VarInt (T.length m + 1))
    putByteString $ encodeUtf8 m
    putByteString $ encodeUtf8 "\n"

