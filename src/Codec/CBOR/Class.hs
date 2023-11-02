{-# LANGUAGE DefaultSignatures #-}

module Codec.CBOR.Class where

import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
import Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import Codec.CBOR.Write (toStrictByteString)
import Data.Map.Lazy qualified as Map
import GHC.Generics
import Prelude hiding (decodeFloat, encodeFloat)

class Serialise a where
  encode :: a -> Encoding
  default encode :: (Generic a, GSerialiseEncode (Rep a)) => a -> Encoding
  encode = gencode . from
  decode :: Decoder s a
  default decode :: (Generic a, GSerialiseDecode (Rep a)) => Decoder s a
  decode = to <$> gdecode

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

-- taken from serialise…

class GSerialiseEncode f where
  gencode :: f a -> Encoding

class GSerialiseDecode f where
  gdecode :: Decoder s (f a)

instance GSerialiseEncode V1 where
  gencode _ = mempty

instance GSerialiseDecode V1 where
  gdecode = error "V1 don't have constructors"

instance GSerialiseEncode U1 where
  gencode _ = mempty

instance GSerialiseDecode U1 where
  gdecode = pure U1

instance (GSerialiseEncode a) => GSerialiseEncode (M1 i c a) where
  gencode = gencode . unM1

instance (GSerialiseDecode a) => GSerialiseDecode (M1 i c a) where
  gdecode = M1 <$> gdecode

instance (Serialise a) => GSerialiseEncode (K1 i a) where
  gencode (K1 a) = encode a

instance (Serialise a) => GSerialiseDecode (K1 i a) where
  gdecode = K1 <$> decode

-- | Serialization of product types
class GSerialiseProd f where
  -- | Number of fields in product type
  nFields :: Proxy f -> Word

  -- | Encode fields sequentially without writing header
  encodeSeq :: f a -> Encoding

  -- | Decode fields sequentially without reading header
  gdecodeSeq :: Decoder s (f a)

-- | @since 0.2.0.0
instance (GSerialiseProd f, GSerialiseProd g) => GSerialiseProd (f :*: g) where
  nFields _ = nFields (Proxy :: Proxy f) + nFields (Proxy :: Proxy g)
  encodeSeq (f :*: g) = encodeSeq f <> encodeSeq g
  gdecodeSeq = do
    !f <- gdecodeSeq
    !g <- gdecodeSeq
    return (f :*: g)

-- | @since 0.2.0.0
instance GSerialiseProd U1 where
  -- N.B. Could only be reached when one of constructors in sum type
  --      don't have parameters
  nFields _ = 0
  encodeSeq _ = mempty
  gdecodeSeq = return U1

-- | @since 0.2.0.0
instance (Serialise a) => GSerialiseProd (K1 i a) where
  -- Ordinary field
  nFields _ = 1
  encodeSeq (K1 f) = encode f
  gdecodeSeq = K1 <$> decode

instance (GSerialiseProd f, GSerialiseProd g) => GSerialiseEncode (f :*: g) where
  -- Products are serialised as N-tuples with 0 constructor tag
  gencode (f :*: g) =
    encodeListLen (nFields (Proxy :: Proxy (f :*: g)))
      <> encodeSeq f
      <> encodeSeq g

instance (GSerialiseProd f, GSerialiseProd g) => GSerialiseDecode (f :*: g) where
  gdecode = do
    let nF = nFields (Proxy :: Proxy (f :*: g))
    n <- decodeListLen
    -- TODO FIXME: signedness of list length
    when (fromIntegral n /= nF) $
      fail $
        "Wrong number of fields: expected="
          ++ show (nF + 1)
          ++ " got="
          ++ show n
    !f <- gdecodeSeq
    !g <- gdecodeSeq
    return $ f :*: g

{- | Serialization of sum types

@since 0.2.0.0
-}
class GSerialiseSum f where
  -- | Number of constructor of given value
  conNumber :: f a -> Word

  -- | Number of fields of given value
  numOfFields :: f a -> Word

  -- | Encode field
  encodeSum :: f a -> Encoding

  -- | Decode field
  decodeSum :: Word -> Decoder s (f a)

  -- | Number of constructors
  nConstructors :: Proxy f -> Word

  -- | Number of fields for given constructor number
  fieldsForCon :: Proxy f -> Word -> Decoder s Word

  maxFields :: Proxy f -> Word

-- | @since 0.2.0.0
instance (GSerialiseSum f, GSerialiseSum g) => GSerialiseSum (f :+: g) where
  conNumber x = case x of
    L1 f -> conNumber f
    R1 g -> conNumber g + nConstructors (Proxy :: Proxy f)
  numOfFields x = case x of
    L1 f -> numOfFields f
    R1 g -> numOfFields g
  encodeSum x = case x of
    L1 f -> encodeSum f
    R1 g -> encodeSum g

  nConstructors _ =
    nConstructors (Proxy :: Proxy f)
      + nConstructors (Proxy :: Proxy g)

  maxFields _ =
    maxFields (Proxy :: Proxy f)
      `max` maxFields (Proxy :: Proxy g)

  fieldsForCon _ n
    | n < nL = fieldsForCon (Proxy :: Proxy f) n
    | otherwise = fieldsForCon (Proxy :: Proxy g) (n - nL)
    where
      nL = nConstructors (Proxy :: Proxy f)

  decodeSum nCon
    | nCon < nL = L1 <$> decodeSum nCon
    | otherwise = R1 <$> decodeSum (nCon - nL)
    where
      nL = nConstructors (Proxy :: Proxy f)

-- | @since 0.2.0.0
instance (i ~ C, GSerialiseProd f) => GSerialiseSum (M1 i c f) where
  conNumber _ = 0
  numOfFields _ = nFields (Proxy :: Proxy f)
  encodeSum (M1 f) = encodeSeq f
  maxFields _ = nFields (Proxy :: Proxy f)

  nConstructors _ = 1
  fieldsForCon _ 0 = return $ nFields (Proxy :: Proxy f)
  fieldsForCon _ _ = fail "Bad constructor number"
  decodeSum 0 = M1 <$> gdecodeSeq
  decodeSum _ = fail "bad constructor number"

-- | @since 0.2.0.0
instance (GSerialiseSum f, GSerialiseSum g) => GSerialiseEncode (f :+: g) where
  -- Sum types are serialised as N-tuples and first element is
  -- constructor tag
  gencode a =
    ( if maxFields (Proxy :: Proxy (f :+: g)) > 0
        then encodeListLen (numOfFields a + 1)
        else mempty
    )
      <> encode (conNumber a)
      <> encodeSum a

-- | @since 0.2.0.0
instance (GSerialiseSum f, GSerialiseSum g) => GSerialiseDecode (f :+: g) where
  gdecode = do
    n <-
      if maxFields (Proxy :: Proxy (f :+: g)) == 0
        then pure 1
        else decodeListLen
    -- TODO FIXME: Again signedness
    when (n == 0) $
      fail "Empty list encountered for sum type"
    nCon <- decodeWord
    trueN <- fieldsForCon (Proxy :: Proxy (f :+: g)) nCon
    when (n - 1 /= fromIntegral trueN) $
      fail $
        "Number of fields mismatch: expected="
          ++ show trueN
          ++ " got="
          ++ show n
    decodeSum nCon
