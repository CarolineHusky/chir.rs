-- | Simple TH code for embedding haskell files with PUA strings into your source
module Text.PUA.TH where

import Language.Haskell.Meta (parseDecs)
import Language.Haskell.TH (Dec, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile)

escapePUA :: Char -> String
escapePUA c
  | (c >= '\xE000' && c <= '\xF8FF')
      || (c >= '\xF0000' && c <= '\xFFFFD')
      || (c >= '\x100000' && c <= '\x10FFFD') =
      '\\' : show (fromEnum c)
  | otherwise = one c

includePUAHS :: FilePath -> Q [Dec]
includePUAHS file = do
  _ <- addDependentFile file
  src <- concatMap escapePUA . (decodeUtf8 :: ByteString -> String) <$> runIO (readFileBS file)
  Right decs <- pure $ parseDecs src
  pure decs
