-- | Simple TH code for embedding haskell files with PUA strings into your source
module Text.PUA.TH where

import Language.Haskell.Meta (parseDecs)
import Language.Haskell.TH (Dec (FunD, SigD), Name, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile)

escapePUA :: Char -> String
escapePUA c
  | (c >= '\xE000' && c <= '\xF8FF')
      || (c >= '\xF0000' && c <= '\xFFFFD')
      || (c >= '\x100000' && c <= '\x10FFFD') =
      '\\' : show (fromEnum c)
  | otherwise = one c

includeWith :: (String -> String) -> FilePath -> Q [Dec]
includeWith f file = do
  _ <- addDependentFile file
  src <- f . (decodeUtf8 :: ByteString -> String) <$> runIO (readFileBS file)
  Right decs <- pure $ parseDecs src
  pure decs

includeWithReplacement :: (Char -> String) -> FilePath -> Q [Dec]
includeWithReplacement = includeWith . concatMap

includePUAHS :: FilePath -> Q [Dec]
includePUAHS = includeWithReplacement escapePUA

replaceFunctionName :: Name -> Name -> Dec -> Dec
replaceFunctionName n1 n2 (FunD n3 cs)
  | n1 == n3 = FunD n2 cs
  | otherwise = FunD n3 cs
replaceFunctionName n1 n2 (SigD n3 ty)
  | n1 == n3 = SigD n2 ty
  | otherwise = SigD n3 ty
replaceFunctionName _ _ v = v

includeWithFunctionRename :: [(Name, Name)] -> (String -> String) -> FilePath -> Q [Dec]
includeWithFunctionRename renames f file = do
  code <- includeWith f file
  pure $ flip (foldr (uncurry replaceFunctionName)) renames <$> code
