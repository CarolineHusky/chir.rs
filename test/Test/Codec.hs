module Test.Codec where

import Test.Codec.CBOR (cborTests)
import Test.Tasty (TestTree, testGroup)

codecTests :: TestTree
codecTests = testGroup "Codec" [cborTests]
