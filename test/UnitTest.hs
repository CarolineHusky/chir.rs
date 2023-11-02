module Main where

import Main.Utf8 (withUtf8)
import Test (tests)
import Test.Tasty (defaultMain)
import Test.Tasty.QuickCheck ()

testMain :: IO ()
testMain = defaultMain tests

main :: IO ()
main = withUtf8 testMain
