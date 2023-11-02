module Test.Codec.CBOR.Class where

import Codec.CBOR.Class (deserialise, serialise)
import Data.ByteString (pack)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary), testProperty)

isEfficientMaybeEncode :: Maybe Int -> Bool
isEfficientMaybeEncode Nothing = serialise (Nothing :: Maybe Int) == pack [0xf6]
isEfficientMaybeEncode (Just a) = serialise (Just a) == serialise a

classTests :: TestTree
classTests =
  testGroup
    "Class"
    [ testProperty "Word serialize" $ \d -> deserialise (serialise (d :: Word)) == Right d
    , testProperty "Word8 serialize" $ \d -> deserialise (serialise (d :: Word8)) == Right d
    , testProperty "Word16 serialize" $ \d -> deserialise (serialise (d :: Word16)) == Right d
    , testProperty "Word32 serialize" $ \d -> deserialise (serialise (d :: Word32)) == Right d
    , testProperty "Word64 serialize" $ \d -> deserialise (serialise (d :: Word64)) == Right d
    , testProperty "Int serialize" $ \d -> deserialise (serialise (d :: Int)) == Right d
    , testProperty "Int8 serialize" $ \d -> deserialise (serialise (d :: Int8)) == Right d
    , testProperty "Int16 serialize" $ \d -> deserialise (serialise (d :: Int16)) == Right d
    , testProperty "Int32 serialize" $ \d -> deserialise (serialise (d :: Int32)) == Right d
    , testProperty "Int64 serialize" $ \d -> deserialise (serialise (d :: Int64)) == Right d
    , testProperty "Integer serialize" $ \d -> deserialise (serialise (d :: Integer)) == Right d
    , testProperty "ByteString serialize" $ \d -> deserialise (serialise (d :: ByteString)) == Right d
    , testProperty "Text serialize" $ \d -> deserialise (serialise (d :: Text)) == Right d
    , testProperty "String serialize" $ \d -> deserialise (serialise (d :: String)) == Right d
    , testProperty "Array serialize" $ \d -> deserialise (serialise (d :: [Int])) == Right d
    , testProperty "Maybe serialize" $ \d -> deserialise (serialise (d :: Maybe Int)) == Right d
    , testProperty "Efficient Maybe encoding" isEfficientMaybeEncode
    , testProperty "Bool serialize" $ \d -> deserialise (serialise (d :: Bool)) == Right d
    , testProperty "Float serialize" $ \d -> deserialise (serialise (d :: Float)) == Right d
    , testProperty "Double serialize" $ \d -> deserialise (serialise (d :: Double)) == Right d
    ]

instance Arbitrary Text where
  arbitrary = (toText :: String -> Text) <$> arbitrary

instance Arbitrary ByteString where
  arbitrary = (pack :: [Word8] -> ByteString) <$> arbitrary
