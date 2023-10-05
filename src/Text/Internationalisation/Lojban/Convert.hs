module Text.Internationalisation.Lojban.Convert where

import Data.Time (Day, toGregorian)
import Text.PUA.TH (includePUAHS)

includePUAHS "src/Text/Internationalisation/Lojban/Convert.phs"

zlrToLatinStr :: String -> String
zlrToLatinStr = concatMap zlrToLatinChar

zlrToLatin :: Text -> Text
zlrToLatin = toText . zlrToLatinStr . toString

formatNumberString :: Text -> Text
formatNumberString = toText . concatMap formatDigit . toString

formatNumber :: (Show a, Num a) => a -> Text
formatNumber = formatNumberString . show
