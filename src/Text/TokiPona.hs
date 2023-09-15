module Text.TokiPona (spToLatin) where

import Text.Parsec (anyChar, char, choice, lookAhead, many1, manyTill, parse, skipMany, try)
import Text.Parsec.Text (Parser)
import Utils (capitalize)
import Prelude hiding (unwords)

matchLogogram :: Parser Char
matchLogogram = do
  nextChar <- lookAhead anyChar
  let charOrd = ord nextChar
  let validCodepoint = ((charOrd >= 0x1F900) && (charOrd <= 0xF1988)) || ((charOrd >= 0xF19A0) && (charOrd <= 0xF19A3)) || charOrd == 0x0F1993
  if validCodepoint then char nextChar else fail "Missing logogram"

logogramToText :: Char -> String
logogramToText '\x0F1900' = "a"
logogramToText '\x0F1901' = "akesi"
logogramToText '\x0F1902' = "ala"
logogramToText '\x0F1903' = "alasa"
logogramToText '\x0F1904' = "ale"
logogramToText '\x0F1905' = "anpa"
logogramToText '\x0F1906' = "ante"
logogramToText '\x0F1907' = "anu"
logogramToText '\x0F1908' = "awen"
logogramToText '\x0F1909' = "e"
logogramToText '\x0F190A' = "en"
logogramToText '\x0F190B' = "esun"
logogramToText '\x0F190C' = "ijo"
logogramToText '\x0F190D' = "ike"
logogramToText '\x0F190E' = "ilo"
logogramToText '\x0F190F' = "insa"
logogramToText '\x0F1910' = "jaki"
logogramToText '\x0F1911' = "jan"
logogramToText '\x0F1912' = "jelo"
logogramToText '\x0F1913' = "jo"
logogramToText '\x0F1914' = "kala"
logogramToText '\x0F1915' = "kalama"
logogramToText '\x0F1916' = "kama"
logogramToText '\x0F1917' = "kasi"
logogramToText '\x0F1918' = "ken"
logogramToText '\x0F1919' = "kepeken"
logogramToText '\x0F191A' = "kili"
logogramToText '\x0F191B' = "kiwen"
logogramToText '\x0F191C' = "ko"
logogramToText '\x0F191D' = "kon"
logogramToText '\x0F191E' = "kule"
logogramToText '\x0F191F' = "kulupu"
logogramToText '\x0F1920' = "kute"
logogramToText '\x0F1921' = "la"
logogramToText '\x0F1922' = "lape"
logogramToText '\x0F1923' = "laso"
logogramToText '\x0F1924' = "lawa"
logogramToText '\x0F1925' = "len"
logogramToText '\x0F1926' = "lete"
logogramToText '\x0F1927' = "li"
logogramToText '\x0F1928' = "lili"
logogramToText '\x0F1929' = "linja"
logogramToText '\x0F192A' = "lipu"
logogramToText '\x0F192B' = "loje"
logogramToText '\x0F192C' = "lon"
logogramToText '\x0F192D' = "luka"
logogramToText '\x0F192E' = "lukin"
logogramToText '\x0F192F' = "lupa"
logogramToText '\x0F1930' = "ma"
logogramToText '\x0F1931' = "mama"
logogramToText '\x0F1932' = "mani"
logogramToText '\x0F1933' = "meli"
logogramToText '\x0F1934' = "mi"
logogramToText '\x0F1935' = "mije"
logogramToText '\x0F1936' = "moku"
logogramToText '\x0F1937' = "moli"
logogramToText '\x0F1938' = "monsi"
logogramToText '\x0F1939' = "mu"
logogramToText '\x0F193A' = "mun"
logogramToText '\x0F193B' = "musi"
logogramToText '\x0F193C' = "mute"
logogramToText '\x0F193D' = "nanpa"
logogramToText '\x0F193E' = "nasa"
logogramToText '\x0F193F' = "nasin"
logogramToText '\x0F1940' = "nena"
logogramToText '\x0F1941' = "ni"
logogramToText '\x0F1942' = "nimi"
logogramToText '\x0F1943' = "noka"
logogramToText '\x0F1944' = "o"
logogramToText '\x0F1945' = "olin"
logogramToText '\x0F1946' = "ona"
logogramToText '\x0F1947' = "open"
logogramToText '\x0F1948' = "pakala"
logogramToText '\x0F1949' = "pali"
logogramToText '\x0F194A' = "palisa"
logogramToText '\x0F194B' = "pan"
logogramToText '\x0F194C' = "pana"
logogramToText '\x0F194D' = "pi"
logogramToText '\x0F194E' = "pilin"
logogramToText '\x0F194F' = "pimeja"
logogramToText '\x0F1950' = "pini"
logogramToText '\x0F1951' = "pipi"
logogramToText '\x0F1952' = "poka"
logogramToText '\x0F1953' = "poki"
logogramToText '\x0F1954' = "pona"
logogramToText '\x0F1955' = "pu"
logogramToText '\x0F1956' = "sama"
logogramToText '\x0F1957' = "seli"
logogramToText '\x0F1958' = "selo"
logogramToText '\x0F1959' = "seme"
logogramToText '\x0F195A' = "sewi"
logogramToText '\x0F195B' = "sijelo"
logogramToText '\x0F195C' = "sike"
logogramToText '\x0F195D' = "sin"
logogramToText '\x0F195E' = "sina"
logogramToText '\x0F195F' = "sinpin"
logogramToText '\x0F1960' = "sitelen"
logogramToText '\x0F1961' = "sona"
logogramToText '\x0F1962' = "soweli"
logogramToText '\x0F1963' = "suli"
logogramToText '\x0F1964' = "suno"
logogramToText '\x0F1965' = "supa"
logogramToText '\x0F1966' = "suwi"
logogramToText '\x0F1967' = "tan"
logogramToText '\x0F1968' = "taso"
logogramToText '\x0F1969' = "tawa"
logogramToText '\x0F196A' = "telo"
logogramToText '\x0F196B' = "tenpo"
logogramToText '\x0F196C' = "toki"
logogramToText '\x0F196D' = "tomo"
logogramToText '\x0F196E' = "tu"
logogramToText '\x0F196F' = "unpa"
logogramToText '\x0F1970' = "uta"
logogramToText '\x0F1971' = "utala"
logogramToText '\x0F1972' = "walo"
logogramToText '\x0F1973' = "wan"
logogramToText '\x0F1974' = "waso"
logogramToText '\x0F1975' = "wawa"
logogramToText '\x0F1976' = "weka"
logogramToText '\x0F1977' = "wile"
logogramToText '\x0F1978' = "namako"
logogramToText '\x0F1979' = "kin"
logogramToText '\x0F197A' = "oko"
logogramToText '\x0F197B' = "kipisi"
logogramToText '\x0F197C' = "leko"
logogramToText '\x0F197D' = "monsuta"
logogramToText '\x0F197E' = "tonsi"
logogramToText '\x0F197F' = "jasima"
logogramToText '\x0F1980' = "kijetesantakalu"
logogramToText '\x0F1981' = "soko"
logogramToText '\x0F1982' = "meso"
logogramToText '\x0F1983' = "epiku"
logogramToText '\x0F1984' = "kokosila"
logogramToText '\x0F1985' = "lanpan"
logogramToText '\x0F1986' = "n"
logogramToText '\x0F1987' = "misikeke"
logogramToText '\x0F1988' = "ku"
logogramToText '\x0F19A0' = "pake"
logogramToText '\x0F19A1' = "apeja"
logogramToText '\x0F19A2' = "majuna"
logogramToText '\x0F19A3' = "powe"
logogramToText '\x0F1993' = "pi"
logogramToText c = [c]

parseLogogram :: Parser String
parseLogogram = logogramToText <$> matchLogogram

parseLogogramFirst :: Parser Char
parseLogogramFirst = do
  transliterated <- parseLogogram
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

parseWord :: Parser String
parseWord = do
  ignoreChars
  try parseCartouche <|> try ((' ' :) <$> parseLogogram) <|> (one <$> anyChar)

parseText :: Parser Text
parseText = toText . concat <$> many parseWord

spToLatin :: Text -> Text
spToLatin t = case parse parseText "" t of
  Left e -> show e
  Right v -> v