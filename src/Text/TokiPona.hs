module Text.TokiPona (spToLatin, spToEmoji, ShowNum (..), formatDate) where

import Data.Time (Day, toGregorian)
import Text.PUA.TH (includePUAHS)
import Text.Parsec (anyChar, char, choice, lookAhead, many1, parse, skipMany, try)
import Text.Parsec.Text (Parser)
import Utils (capitalize)
import Prelude hiding (unwords)

includePUAHS "src/Text/TokiPona.phs"

matchLogogram :: Parser Char
matchLogogram = do
  nextChar <- lookAhead anyChar
  let charOrd = ord nextChar
  let validCodepoint = ((charOrd >= 0x1F900) && (charOrd <= 0xF1988)) || ((charOrd >= 0xF19A0) && (charOrd <= 0xF19A3)) || charOrd == 0x0F1993 || ((charOrd > 0xff900) && (charOrd <= 0xffbff))
  if validCodepoint then char nextChar else fail "Missing logogram"

parseLogogram :: (Char -> String) -> Parser String
parseLogogram p = p <$> matchLogogram

parseLogogramFirst :: Parser Char
parseLogogramFirst = do
  ignoreChars
  transliterated <- parseLogogram logogramToText
  case uncons transliterated of
    Just (c, _) -> return c
    Nothing -> fail "Program bug: parseLogogram returned empty string"

matchChar :: Char -> Parser ()
matchChar c = char c >> pass

withOptionalSeparator :: Parser a -> Parser () -> Parser a
withOptionalSeparator parser separator = do
  res <- parser
  try separator <|> pass
  return res

parseCartouche :: Parser String
parseCartouche = do
  _ <- char '\xF1990'
  cartoucheChars <- many1 $ withOptionalSeparator parseLogogramFirst $ matchChar '\xF1992'
  _ <- char '\xF1991'
  return $ ' ' : capitalize cartoucheChars

ignoredChars :: Parser Char
ignoredChars = choice (map char "\xF1994\xF1995\xF1996\xF1997\xF1998\xF1999\xF199A\xF199B\xF199C\xF199D")

ignoreChars :: Parser ()
ignoreChars = skipMany ignoredChars

withOptSpace :: (Functor f) => Bool -> f String -> f String
withOptSpace False = id
withOptSpace True = (<$>) (' ' :)

parseWord :: Bool -> (Char -> String) -> Parser String
parseWord space p = do
  ignoreChars
  try parseCartouche <|> try (withOptSpace space $ parseLogogram p) <|> (one <$> anyChar)

parseText :: Bool -> (Char -> String) -> Parser Text
parseText space p = toText . concat <$> many (parseWord space p)

spToText :: Bool -> (Char -> String) -> Text -> Text
spToText space p t = case parse (parseText space p) "" t of
  Left e -> show e
  Right v -> v

spToLatin :: Text -> Text
spToLatin = spToText True logogramToText

spToEmoji :: Text -> Text
spToEmoji = spToText False logogramToEmoji

class (Num a) => ShowNum a where
  formatNum :: a -> Text

instance {-# OVERLAPPING #-} ShowNum Integer where
  formatNum :: Integer -> Text
  formatNum = toText . reverse . formatInteger'
    where
      formatInteger' n
        | n < 0 = '\x0F1976' : formatInteger' (-n)
        | n < 6 = [formatDigit n]
        | otherwise = formatDigit (n `mod` 6) : formatInteger' (n `div` 6)

instance (Integral a, ShowNum a) => ShowNum (Ratio a) where
  formatNum :: Ratio a -> Text
  formatNum v = formatNum (numerator v) <> "\x0F197B" <> formatNum (denominator v)

instance (Integral a, Num a) => ShowNum a where
  formatNum :: a -> Text
  formatNum = formatNum . (fromIntegral :: a -> Integer)
