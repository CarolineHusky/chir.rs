module Codec.CBOR.Class where

import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
import Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString, toStrictByteString)
import Data.Map.Lazy qualified as Map
import Prelude hiding (decodeFloat, encodeFloat)

class Serialise a where
  encode :: a -> Encoding
  decode :: Decoder s a

instance Serialise Word where
  encode = encodeWord
  decode = decodeWord

instance Serialise Word8 where
  encode = encodeWord8
  decode = decodeWord8

instance Serialise Word16 where
  encode = encodeWord16
  decode = decodeWord16

instance Serialise Word32 where
  encode = encodeWord32
  decode = decodeWord32

instance Serialise Word64 where
  encode = encodeWord64
  decode = decodeWord64

instance Serialise Int where
  encode = encodeInt
  decode = decodeInt

instance Serialise Int8 where
  encode = encodeInt8
  decode = decodeInt8

instance Serialise Int16 where
  encode = encodeInt16
  decode = decodeInt16

instance Serialise Int32 where
  encode = encodeInt32
  decode = decodeInt32

instance Serialise Int64 where
  encode = encodeInt64
  decode = decodeInt64

instance Serialise Integer where
  encode = encodeInteger
  decode = decodeInteger

instance Serialise ByteString where
  encode = encodeBytes
  decode = decodeBytes

instance Serialise Text where
  encode = encodeString
  decode = decodeString

instance {-# OVERLAPS #-} Serialise String where
  encode = encodeString . toText
  decode = toString <$> decodeString

instance (Serialise a) => Serialise [a] where
  encode [] = encodeListLen 0
  encode xs = encodeListLenIndef <> foldr ((<>) . encode) mempty xs <> encodeBreak

  decode = do
    mn <- decodeListLenOrIndef
    case mn of
      Nothing -> decodeSequenceLenIndef (flip (:)) [] reverse decode
      Just n -> decodeSequenceLenN (flip (:)) [] reverse n decode

instance (Serialise a) => Serialise (Maybe a) where
  encode Nothing = encodeNull
  encode (Just x) = encode x

  decode = do
    token <- peekTokenType
    case token of
      TypeNull -> decodeNull >> pure Nothing
      _ -> Just <$> decode

instance Serialise Bool where
  encode = encodeBool
  decode = decodeBool

instance Serialise Float where
  encode = encodeFloat
  decode = decodeFloat

instance Serialise Double where
  encode = encodeDouble
  decode = decodeDouble

instance (Serialise a, Serialise b) => Serialise (a, b) where
  encode (a, b) = encode a <> encode b
  decode = do
    a <- decode
    b <- decode
    pure (a, b)

instance (Ord a, Serialise a, Serialise b) => Serialise (Map a b) where
  encode m = encodeMapLenIndef <> Map.foldrWithKey (\a b r -> r <> encode a <> encode b) mempty m <> encodeBreak
  decode = do
    mn <- decodeMapLenOrIndef
    kvList <- case mn of
      Nothing -> decodeSequenceLenIndef (flip (:)) [] reverse decode
      Just n -> decodeSequenceLenN (flip (:)) [] reverse n decode
    pure $ Map.fromList kvList

serialise :: (Serialise a) => a -> ByteString
serialise = toStrictByteString . encode

deserialise :: (Serialise a) => ByteString -> Either DeserialiseFailure a
deserialise = second snd . deserialiseFromBytes decode . toLazy
