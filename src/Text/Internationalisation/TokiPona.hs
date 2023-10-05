module Text.Internationalisation.TokiPona where

import Language.Haskell.TH (mkName)
import Text.Internationalisation.TokiPona.Convert (mkCartouche, spToEmoji, spToLatin)
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

translateSp :: Message -> WidgetFor a (Either TranslationError Html)
translateSp = translateSp' mkCartouche
translateSe :: Message -> WidgetFor a (Either TranslationError Html)
translateSe = translateSe' (toText . capitalize . toString)
translateSl :: Message -> WidgetFor a (Either TranslationError Html)
translateSl = translateSl' (toText . capitalize . toString)
