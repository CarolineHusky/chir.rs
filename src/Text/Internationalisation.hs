module Text.Internationalisation (
  module Text.Internationalisation.Types,
  __,
  getTranslation,
) where

import Text.Internationalisation.Deutsch qualified as Deutsch
import Text.Internationalisation.English qualified as English
import Text.Internationalisation.Francais qualified as Français
import Text.Internationalisation.Lojban qualified as Lojban
import Text.Internationalisation.Nederlands qualified as Nederlands
import Text.Internationalisation.TokiPona qualified as TokiPona
import Text.Internationalisation.Types
import Utils ((?!))
import Yesod (Html, languages, toHtml, toWidget)
import Yesod.Core (WidgetFor)

translateMessageToLang :: Language -> Message -> WidgetFor a (Either TranslationError Html)
translateMessageToLang English = English.translate
translateMessageToLang Français = Français.translate
translateMessageToLang Deutsch = Deutsch.translate
translateMessageToLang (Lojban LojbanLatin) = Lojban.translateLtn
translateMessageToLang (Lojban LojbanZbalermorna) = Lojban.translateZlr
translateMessageToLang Nederlands = Nederlands.translate
translateMessageToLang (TokiPona SitelenPona) = TokiPona.translateSp
translateMessageToLang (TokiPona SitelenEmosi) = TokiPona.translateSe
translateMessageToLang (TokiPona SitelenLasina) = TokiPona.translateSl

translateMessage :: [Language] -> Message -> WidgetFor a Html
translateMessage ls msg =
  foldr
    (\l -> (?!) (translateMessageToLang l msg))
    (pure (toHtml $ (show :: Message -> Text) msg))
    (ls ++ [English])

getTranslation :: Message -> WidgetFor a Html
getTranslation message = do
  langs <- languages
  translateMessage (parseLanguage . toString <$> langs) message

__ :: Message -> WidgetFor a ()
__ message = getTranslation message >>= toWidget