module Text.Internationalisation.TokiPona.Common where

import Data.Time (Day, toGregorian)
import Text.PUA.TH (includePUAHS)
import Text.Parsec (anyChar, char, choice, lookAhead, many1, parse, skipMany, try)
import Text.Parsec.String (Parser)
import Utils (capitalize, intersperseAfter)
import Prelude hiding (unwords)

includePUAHS "src/Text/Internationalisation/TokiPona/Common.phs"

class (Num a) => ShowNum a where
  formatNum :: a -> String

instance {-# OVERLAPPING #-} ShowNum Integer where
  formatNum :: Integer -> String
  formatNum = reverse . formatInteger'
    where
      formatInteger' n
        | n < 0 = '\x0F1976' : formatInteger' (-n)
        | n < 6 = [formatDigit n]
        | otherwise = formatDigit (n `mod` 6) : formatInteger' (n `div` 6)

instance (Integral a, ShowNum a) => ShowNum (Ratio a) where
  formatNum :: Ratio a -> String
  formatNum v = formatNum (numerator v) <> "\x0F197B" <> formatNum (denominator v)

instance (Integral a, Num a) => ShowNum a where
  formatNum :: a -> String
  formatNum = formatNum . (fromIntegral :: a -> Integer)

matchLogogram :: Parser Char
matchLogogram = do
  nextChar <- lookAhead anyChar
  let charOrd = ord nextChar
  let validCodepoint = ((charOrd >= 0x1F900) && (charOrd <= 0xF1988)) || ((charOrd >= 0xF19A0) && (charOrd <= 0xF19A3)) || charOrd == 0x0F1993 || ((charOrd > 0xff900) && (charOrd <= 0xffbff))
  if validCodepoint then char nextChar else fail "Missing logogram"

parseLogogram :: (Char -> String) -> Parser String
parseLogogram p = p <$> matchLogogram

cartoucheCharacters :: Parser Char
cartoucheCharacters = try matchLogogram <|> try (char '\xF199C') <|> char '\xF199D'

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
  cartoucheChars <- many1 $ withOptionalSeparator cartoucheCharacters $ matchChar '\xF1992'
  _ <- char '\xF1991'
  return $ ' ' : capitalize (fromCartouche cartoucheChars)

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

parseText :: Bool -> (Char -> String) -> Parser String
parseText space p = concat <$> many (parseWord space p)

spToString :: Bool -> (Char -> String) -> String -> String
spToString space p t = case parse (parseText space p) "" t of
  Left e -> show e
  Right v -> v