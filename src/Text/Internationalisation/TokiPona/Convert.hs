module Text.Internationalisation.TokiPona.Convert where

import Data.Time (Day, toGregorian)
import Text.PUA.TH (includePUAHS)
import Text.Parsec (anyChar, char, choice, lookAhead, many1, parse, skipMany, try)
import Text.Parsec.String (Parser)
import Utils (capitalize, intersperseAfter)
import Prelude hiding (unwords)

includePUAHS "src/Text/Internationalisation/TokiPona/Convert.phs"

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

spToLatin :: String -> String
spToLatin = spToString True logogramToText

spToEmoji :: String -> String
spToEmoji = spToString False logogramToEmoji

spToMunjan :: String -> String
spToMunjan = spToString False logogramToHanzi

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

mkCartouche :: Text -> Text
mkCartouche t =
  let cartouche = intersperseAfter '\xF1992' $ toCartouche $ toString t
   in toText $ '\xF1990' : cartouche ++ "\xF1991"

latinToHiragana :: String -> String
latinToHiragana [] = []
latinToHiragana ('a' : r) = 'あ' : latinToHiragana r
latinToHiragana ('i' : r) = 'い' : latinToHiragana r
latinToHiragana ('u' : r) = 'う' : latinToHiragana r
latinToHiragana ('e' : r) = 'え' : latinToHiragana r
latinToHiragana ('o' : r) = 'お' : latinToHiragana r
latinToHiragana ('k' : 'a' : r) = 'か' : latinToHiragana r
latinToHiragana ('k' : 'i' : r) = 'き' : latinToHiragana r
latinToHiragana ('k' : 'u' : r) = 'く' : latinToHiragana r
latinToHiragana ('k' : 'e' : r) = 'け' : latinToHiragana r
latinToHiragana ('k' : 'o' : r) = 'こ' : latinToHiragana r
latinToHiragana ('s' : 'a' : r) = 'さ' : latinToHiragana r
latinToHiragana ('s' : 'i' : r) = 'し' : latinToHiragana r
latinToHiragana ('s' : 'u' : r) = 'す' : latinToHiragana r
latinToHiragana ('s' : 'e' : r) = 'せ' : latinToHiragana r
latinToHiragana ('s' : 'o' : r) = 'そ' : latinToHiragana r
latinToHiragana ('t' : 'a' : r) = 'た' : latinToHiragana r
latinToHiragana ('t' : 'u' : r) = 'つ' : latinToHiragana r
latinToHiragana ('t' : 'e' : r) = 'て' : latinToHiragana r
latinToHiragana ('t' : 'o' : r) = 'と' : latinToHiragana r
latinToHiragana ('m' : 'a' : r) = 'ま' : latinToHiragana r
latinToHiragana ('m' : 'i' : r) = 'み' : latinToHiragana r
latinToHiragana ('m' : 'u' : r) = 'む' : latinToHiragana r
latinToHiragana ('m' : 'e' : r) = 'め' : latinToHiragana r
latinToHiragana ('m' : 'o' : r) = 'も' : latinToHiragana r
latinToHiragana ('p' : 'a' : r) = 'ぱ' : latinToHiragana r
latinToHiragana ('p' : 'i' : r) = 'ぴ' : latinToHiragana r
latinToHiragana ('p' : 'u' : r) = 'ぷ' : latinToHiragana r
latinToHiragana ('p' : 'e' : r) = 'ぺ' : latinToHiragana r
latinToHiragana ('p' : 'o' : r) = 'ぽ' : latinToHiragana r
latinToHiragana ('n' : 'a' : r) = 'な' : latinToHiragana r
latinToHiragana ('n' : 'i' : r) = 'に' : latinToHiragana r
latinToHiragana ('n' : 'u' : r) = 'ぬ' : latinToHiragana r
latinToHiragana ('n' : 'e' : r) = 'ね' : latinToHiragana r
latinToHiragana ('n' : 'o' : r) = 'の' : latinToHiragana r
latinToHiragana ('l' : 'a' : r) = 'ら' : latinToHiragana r
latinToHiragana ('l' : 'i' : r) = 'り' : latinToHiragana r
latinToHiragana ('l' : 'u' : r) = 'る' : latinToHiragana r
latinToHiragana ('l' : 'e' : r) = 'れ' : latinToHiragana r
latinToHiragana ('l' : 'o' : r) = 'ろ' : latinToHiragana r
latinToHiragana ('j' : 'a' : r) = 'や' : latinToHiragana r
latinToHiragana ('j' : 'u' : r) = 'ゆ' : latinToHiragana r
latinToHiragana ('j' : 'e' : r) = 'い' : 'ぇ' : latinToHiragana r
latinToHiragana ('j' : 'o' : r) = 'よ' : latinToHiragana r
latinToHiragana ('w' : 'a' : r) = 'わ' : latinToHiragana r
latinToHiragana ('w' : 'i' : r) = 'う' : 'ぃ' : latinToHiragana r
latinToHiragana ('w' : 'e' : r) = 'う' : 'ぇ' : latinToHiragana r
latinToHiragana ('n' : r) = 'ん' : latinToHiragana r
latinToHiragana (c : r) = c : latinToHiragana r

latinToKatakana :: String -> String
latinToKatakana [] = []
latinToKatakana ('a' : r) = 'ア' : latinToKatakana r
latinToKatakana ('i' : r) = 'イ' : latinToKatakana r
latinToKatakana ('u' : r) = 'ウ' : latinToKatakana r
latinToKatakana ('e' : r) = 'エ' : latinToKatakana r
latinToKatakana ('o' : r) = 'オ' : latinToKatakana r
latinToKatakana ('k' : 'a' : r) = 'カ' : latinToKatakana r
latinToKatakana ('k' : 'i' : r) = 'キ' : latinToKatakana r
latinToKatakana ('k' : 'u' : r) = 'ク' : latinToKatakana r
latinToKatakana ('k' : 'e' : r) = 'ケ' : latinToKatakana r
latinToKatakana ('k' : 'o' : r) = 'コ' : latinToKatakana r
latinToKatakana ('s' : 'a' : r) = 'サ' : latinToKatakana r
latinToKatakana ('s' : 'i' : r) = 'シ' : latinToKatakana r
latinToKatakana ('s' : 'u' : r) = 'ス' : latinToKatakana r
latinToKatakana ('s' : 'e' : r) = 'セ' : latinToKatakana r
latinToKatakana ('s' : 'o' : r) = 'ソ' : latinToKatakana r
latinToKatakana ('t' : 'a' : r) = 'タ' : latinToKatakana r
latinToKatakana ('t' : 'u' : r) = 'ツ' : latinToKatakana r
latinToKatakana ('t' : 'e' : r) = 'テ' : latinToKatakana r
latinToKatakana ('t' : 'o' : r) = 'ト' : latinToKatakana r
latinToKatakana ('m' : 'a' : r) = 'マ' : latinToKatakana r
latinToKatakana ('m' : 'i' : r) = 'ミ' : latinToKatakana r
latinToKatakana ('m' : 'u' : r) = 'ム' : latinToKatakana r
latinToKatakana ('m' : 'e' : r) = 'メ' : latinToKatakana r
latinToKatakana ('m' : 'o' : r) = 'モ' : latinToKatakana r
latinToKatakana ('p' : 'a' : r) = 'パ' : latinToKatakana r
latinToKatakana ('p' : 'i' : r) = 'ピ' : latinToKatakana r
latinToKatakana ('p' : 'u' : r) = 'プ' : latinToKatakana r
latinToKatakana ('p' : 'e' : r) = 'ペ' : latinToKatakana r
latinToKatakana ('p' : 'o' : r) = 'ポ' : latinToKatakana r
latinToKatakana ('n' : 'a' : r) = 'ナ' : latinToKatakana r
latinToKatakana ('n' : 'i' : r) = 'ニ' : latinToKatakana r
latinToKatakana ('n' : 'u' : r) = 'ヌ' : latinToKatakana r
latinToKatakana ('n' : 'e' : r) = 'ネ' : latinToKatakana r
latinToKatakana ('n' : 'o' : r) = 'ノ' : latinToKatakana r
latinToKatakana ('l' : 'a' : r) = 'ラ' : latinToKatakana r
latinToKatakana ('l' : 'i' : r) = 'リ' : latinToKatakana r
latinToKatakana ('l' : 'u' : r) = 'ル' : latinToKatakana r
latinToKatakana ('l' : 'e' : r) = 'レ' : latinToKatakana r
latinToKatakana ('l' : 'o' : r) = 'ロ' : latinToKatakana r
latinToKatakana ('j' : 'a' : r) = 'ヤ' : latinToKatakana r
latinToKatakana ('j' : 'u' : r) = 'ユ' : latinToKatakana r
latinToKatakana ('j' : 'e' : r) = 'イ' : 'ェ' : latinToKatakana r
latinToKatakana ('j' : 'o' : r) = 'ヨ' : latinToKatakana r
latinToKatakana ('w' : 'a' : r) = 'ワ' : latinToKatakana r
latinToKatakana ('w' : 'i' : r) = 'ウ' : 'ィ' : latinToKatakana r
latinToKatakana ('w' : 'e' : r) = 'ウ' : 'ィ' : latinToKatakana r
latinToKatakana ('n' : r) = 'ン' : latinToKatakana r
latinToKatakana (c : r) = c : latinToKatakana r
