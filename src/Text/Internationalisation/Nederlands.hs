{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Text.Internationalisation.Nederlands where

import Text.Internationalisation.Types (
  Message (..),
  TranslationError,
  text,
  untranslated,
 )
import Yesod (Html, WidgetFor)

translate :: Message -> WidgetFor a (Either TranslationError Html)
translate MsgAutomatic = text "Automatisch"
translate MsgFooterText = text "Gemaakt met koffie in Haskell."
translate MsgJumpToContent = text "Direct naar inhoud…"
translate MsgPageBlog = text "Blog"
translate MsgPageChat = text "Chat"
translate MsgPageHome = text "Homepage"
translate MsgPageSubtitle = text "Wasberen? Op het internet? Je zou het bijna niet geloven!"
translate MsgPageTitle = text "Lotte’s Zolder"
translate MsgSettingDarkTheme = text "Donker thema"
translate MsgSettingLanguage = text "Taal:"
translate MsgSettingLightTheme = text "Licht thema"
translate MsgSettingsCookieNotice = text "Wij gebruiken cookies om gebruikersinstellingen bij te houden"
translate MsgSettingsMenu = text "Instellingen"
translate MsgSettingTheme = text "Thema:"
translate _ = untranslated
