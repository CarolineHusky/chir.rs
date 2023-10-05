{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Text.Internationalisation.Francais where

import Text.Internationalisation.Types (
  Message (..),
  TranslationError,
  text,
  untranslated,
 )
import Yesod (Html, WidgetFor)

translate :: Message -> WidgetFor a (Either TranslationError Html)
translate MsgAutomatic = text "Automatique"
translate MsgFooterText = text "Fait en Haskell avec du café."
translate MsgJumpToContent = text "Vers le contenu…"
translate MsgPageBlog = text "Blogue"
translate MsgPageChat = text "Tchat"
translate MsgPageHome = text "Page principale"
translate MsgPageSubtitle = text "Ratons laveurs? Sur l'internet? On ne sait jamais..."
translate MsgPageTitle = text "Le grenier de Lotte"
translate MsgSettingDarkTheme = text "Theme nuit"
translate MsgSettingLightTheme = text "Theme clair"
translate MsgSettingsCookieNotice = text "Nous utilisont des Cookies pour transmettre et enregistrer les préférences d'utilisateur"
translate MsgSettingsMenu = text "Preferences"
translate _ = untranslated