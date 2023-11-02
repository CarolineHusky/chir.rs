module Test.Codec.CBOR where

import Test.Codec.CBOR.Class (classTests)
import Test.Tasty (TestTree, testGroup)

cborTests :: TestTree
cborTests = testGroup "CBOR" [classTests]
