module Text.Internationalisation.Lojban where

import Language.Haskell.TH (mkName)
import Text.Internationalisation.Lojban.Convert (zlrToLatinStr)
import Text.Internationalisation.Types (
  Message (..),
  TranslationError,
  html,
  text,
  untranslated,
 )
import Text.PUA.TH (escapePUA, includeWithFunctionRename)
import Yesod (Html, WidgetFor)

includeWithFunctionRename [(mkName "translate", mkName "translateZlr")] (concatMap escapePUA) "./src/Text/Internationalisation/Lojban.phs"
includeWithFunctionRename [(mkName "translate", mkName "translateLtn")] zlrToLatinStr "./src/Text/Internationalisation/Lojban.phs"
