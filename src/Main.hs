module Main (main) where

import Application (appMain)
import Main.Utf8 (withUtf8)

{- |
 Main entry point.

 The `just run` script will invoke this function.
-}
main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  withUtf8 appMain
