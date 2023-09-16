module Text.Lojban where

-- | Converts a zbalermorna character into a latin character.
zlrToLatinChar :: Char -> String
zlrToLatinChar '\xED80' = "p"
zlrToLatinChar '\xED81' = "t"
zlrToLatinChar '\xED82' = "k"
zlrToLatinChar '\xED83' = "f"
zlrToLatinChar '\xED84' = "l"
zlrToLatinChar '\xED85' = "s"
zlrToLatinChar '\xED86' = "c"
zlrToLatinChar '\xED87' = "m"
zlrToLatinChar '\xED88' = "x"
zlrToLatinChar '\xED89' = "."
zlrToLatinChar '\xED8A' = "'"
zlrToLatinChar '\xED8B' = "[cnima'o bu]"
zlrToLatinChar '\xED8C' = "\x0317" -- accent up
zlrToLatinChar '\xED8D' = "\x0316" -- accent down
zlrToLatinChar '\xED8E' = "\x032D" -- accent up-down
zlrToLatinChar '\xED8F' = "\x032C" -- accent down-up
zlrToLatinChar '\xED90' = "b"
zlrToLatinChar '\xED91' = "d"
zlrToLatinChar '\xED92' = "g"
zlrToLatinChar '\xED93' = "v"
zlrToLatinChar '\xED94' = "r"
zlrToLatinChar '\xED95' = "z"
zlrToLatinChar '\xED96' = "j"
zlrToLatinChar '\xED97' = "n"
zlrToLatinChar '\xED98' = "\x0300" -- manual stress
zlrToLatinChar '\xED99' = "…" -- Pause
zlrToLatinChar '\xED9A' = ","
zlrToLatinChar '\xED9B' = "~" -- stretch mark
zlrToLatinChar '\xEDA0' = "a"
zlrToLatinChar '\xEDA1' = "e"
zlrToLatinChar '\xEDA2' = "i"
zlrToLatinChar '\xEDA3' = "o"
zlrToLatinChar '\xEDA4' = "u"
zlrToLatinChar '\xEDA5' = "y"
zlrToLatinChar '\xEDA6' = "ai"
zlrToLatinChar '\xEDA7' = "ei"
zlrToLatinChar '\xEDA8' = "oi"
zlrToLatinChar '\xEDA9' = "au"
zlrToLatinChar '\xEDAA' = "ǐ"
zlrToLatinChar '\xEDAB' = "ǔ"
zlrToLatinChar '\xEDB0' = "a,"
zlrToLatinChar '\xEDB1' = "e,"
zlrToLatinChar '\xEDB2' = "i,"
zlrToLatinChar '\xEDB3' = "o,"
zlrToLatinChar '\xEDB4' = "u,"
zlrToLatinChar '\xEDB5' = "y,"
zlrToLatinChar '\xEDB6' = "ai,"
zlrToLatinChar '\xEDB7' = "ei,"
zlrToLatinChar '\xEDB8' = "oi,"
zlrToLatinChar '\xEDB9' = "au,"
zlrToLatinChar c = [c]

zlrToLatinStr :: String -> String
zlrToLatinStr = concatMap zlrToLatinChar

zlrToLatin :: Text -> Text
zlrToLatin = toText . zlrToLatinStr . toString

formatDigit :: Char -> String
formatDigit '0' = "\xED97\xEDA3"
formatDigit '1' = "\xED80\xEDA0"
formatDigit '2' = "\xED94\xEDA1"
formatDigit '3' = "\xED86\xEDA2"
formatDigit '4' = "\xED93\xEDA3"
formatDigit '5' = "\xED87\xEDA4"
formatDigit '6' = "\xED88\xEDA0"
formatDigit '7' = "\xED95\xEDA1"
formatDigit '8' = "\xED90\xEDA2"
formatDigit '9' = "\xED85\xEDA3"
formatDigit '+' = "\xED87\xEDA0\xED8A\xEDA4"
formatDigit '-' = "\xED97\xEDA2\xED8A\xEDA4"
formatDigit '.' = "\xED80\xEDA2"
formatDigit ',' = "\xED82\xEDA2\xED8A\xEDA3"
formatDigit 'e' = "\xED80\xEDA2\xED8A\xEDA2\xED80\xEDA0\xED97\xEDA3\xED81\xEDA1\xED8A\xEDA0"
formatDigit c = one c

formatNumberString :: Text -> Text
formatNumberString = toText . concatMap formatDigit . toString

formatNumber :: (Show a, Num a) => a -> Text
formatNumber = formatNumberString . show
