module Text.Internationalisation.TokiPona where

import Language.Haskell.TH (mkName)
import Text.Internationalisation.TokiPona.SitelenEmosi qualified as SitelenEmosi
import Text.Internationalisation.TokiPona.SitelenLasina qualified as SitelenLasina
import Text.Internationalisation.TokiPona.SitelenMunjan qualified as SitelenMunjan
import Text.Internationalisation.TokiPona.SitelenPona qualified as SitelenPona
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

includeWithFunctionRename [(mkName "translate", mkName "translateSp'")] (concatMap escapePUA . SitelenPona.fromSitelenPona) "./src/Text/Internationalisation/TokiPona.phs"
includeWithFunctionRename [(mkName "translate", mkName "translateSe'")] SitelenEmosi.fromSitelenPona "./src/Text/Internationalisation/TokiPona.phs"
includeWithFunctionRename [(mkName "translate", mkName "translateSl'")] SitelenLasina.fromSitelenPona "./src/Text/Internationalisation/TokiPona.phs"
includeWithFunctionRename [(mkName "translate", mkName "translateSm'")] SitelenMunjan.fromSitelenPona "./src/Text/Internationalisation/TokiPona.phs"

translateSp :: Message -> WidgetFor a (Either TranslationError Html)
translateSp = translateSp' SitelenPona.cartouche
translateSe :: Message -> WidgetFor a (Either TranslationError Html)
translateSe = translateSe' SitelenEmosi.cartouche
translateSl :: Message -> WidgetFor a (Either TranslationError Html)
translateSl = translateSl' SitelenLasina.cartouche
translateSm :: Message -> WidgetFor a (Either TranslationError Html)
translateSm = translateSm' SitelenMunjan.cartouche
