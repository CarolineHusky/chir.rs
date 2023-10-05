module Text.Internationalisation.TokiPona where

import Language.Haskell.TH (mkName)
import Text.Internationalisation.TokiPona.Convert (latinToKatakana, mkCartouche, spToEmoji, spToLatin, spToMunjan)
import Text.Internationalisation.Types (
  Message (..),
  TranslationError,
  html,
  text,
  untranslated,
 )
import Text.PUA.TH (escapePUA, includeWithFunctionRename)
import Utils (capitalize)
import Yesod (Html, WidgetFor)

includeWithFunctionRename [(mkName "translate", mkName "translateSp'")] (concatMap escapePUA) "./src/Text/Internationalisation/TokiPona.phs"
includeWithFunctionRename [(mkName "translate", mkName "translateSe'")] spToEmoji "./src/Text/Internationalisation/TokiPona.phs"
includeWithFunctionRename [(mkName "translate", mkName "translateSl'")] spToLatin "./src/Text/Internationalisation/TokiPona.phs"
includeWithFunctionRename [(mkName "translate", mkName "translateSm'")] spToMunjan "./src/Text/Internationalisation/TokiPona.phs"

smCartouche :: Text -> Text
smCartouche "alenta" = "亞特蘭大"
smCartouche "elopa" = "歐羅巴"
smCartouche "epanja" = "西班牙"
smCartouche "epelanto" = "希望"
smCartouche "inli" = "英吉利"
smCartouche "italija" = "意大利"
smCartouche "kanata" = "加拿大"
smCartouche "kanse" = "法蘭西"
smCartouche "lantan" = "倫敦"
smCartouche "loma" = "羅馬"
smCartouche "mesiko" = "墨西哥"
smCartouche "munjan" = "文言"
smCartouche "nijon" = "日本"
smCartouche "oselija" = "澳大利亞"
smCartouche "perlin" = "柏林"
smCartouche "sonko" = "中國"
smCartouche "tosi" = "德意志"
smCartouche l = toText $ latinToKatakana $ toString l

translateSp :: Message -> WidgetFor a (Either TranslationError Html)
translateSp = translateSp' mkCartouche
translateSe :: Message -> WidgetFor a (Either TranslationError Html)
translateSe = translateSe' (toText . capitalize . toString)
translateSl :: Message -> WidgetFor a (Either TranslationError Html)
translateSl = translateSl' (toText . capitalize . toString)
translateSm :: Message -> WidgetFor a (Either TranslationError Html)
translateSm = translateSm' smCartouche