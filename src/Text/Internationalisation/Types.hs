-- | Shared types for the internationalisation system
module Text.Internationalisation.Types where

import Text.Blaze.Html (Html, preEscapedToHtml, toHtml)
import Yesod (WidgetFor)

-- | Errors occured while translation
data TranslationError
  = -- | Not translated into this language
    Untranslated
  deriving stock (Typeable, Show)

instance Exception TranslationError

-- | Message Type
data Message
  = -- | Service Name Akkoma
    MsgAkkoma
  | -- | Alternate link for the same resource
    MsgAlternateLink
  | -- | Non exhaustive list on homepage
    MsgAndALotMore
  | -- | Authorization command on login page
    MsgAuthorize
  | -- | Automatic parameter selection
    MsgAutomatic
  | -- | Forgejo service name
    MsgForgejo
  | -- | Github service name
    MsgGithub
  | -- | Link to GPG key after email
    MsgGPGKeyNote
  | -- | Footer text
    MsgFooterText
  | -- | Homepage footnote about certain chats being bridged to matrix
    MsgFootnoteBridgedMatrix
  | -- | Alt text to the image on the homepage
    MsgImgVintagecoyotePrideicon
  | -- | Image credits to the image on the homepage
    MsgImgVintagecoyotePrideiconFigcaption
  | -- | Screenreader link that jumps to the main content on the page
    MsgJumpToContent
  | -- | My discord username
    MsgMyDiscord
  | -- | My Email Address
    MsgMyEmail
  | -- | My fediverse account
    MsgMyFediverse
  | -- | My forgejo instance
    MsgMyForgejo
  | -- | My Keybase account
    MsgMyKeybase
  | -- | My matrix chat handle
    MsgMyMatrixChat
  | -- | signal/whatsapp disclaimer
    MsgMySignalWhatsapp
  | -- | my telegram chat handle
    MsgMyTelegram
  | -- | Nextcloud service name
    MsgNextcloud
  | -- | Blog page link
    MsgPageBlog
  | -- | Chat page link
    MsgPageChat
  | -- | Homepage link
    MsgPageHome
  | -- | Site subtitle
    MsgPageSubtitle
  | -- | Site title
    MsgPageTitle
  | -- | Peertube service name
    MsgPeertube
  | -- | Title for the social media list
    MsgReportedSightings
  | -- | Subtitle for the social media list
    MsgReportedSightingsSubtitle
  | -- | Site index page
    MsgSiteIndex
  | -- | Self intro part 1
    MsgSelfIntroPart1
  | -- | Self intro part 2
    MsgSelfIntroPart2
  | -- | Self intro part 3
    MsgSelfIntroPart3
  | -- | Self intro part 4
    MsgSelfIntroPart4
  | -- | Settings option dark theme
    MsgSettingDarkTheme
  | -- | Settings label for language
    MsgSettingLanguage
  | -- | Setting option for light theme
    MsgSettingLightTheme
  | -- | Cookie notice in settings menu
    MsgSettingsCookieNotice
  | -- | Settings menu label
    MsgSettingsMenu
  | -- | Settings label for theme
    MsgSettingTheme
  | -- | Username label for login
    MsgUsername
  deriving stock (Show)

-- language specific labels below

-- | Toki Pona scripts
data TokiPonaScripts
  = -- | The standard latin orthography
    SitelenLasina
  | -- | Sitelen Pona pictographic script
    SitelenPona
  | -- | Emoji pictographic script
    SitelenEmosi
  | -- | Classical chinese styled script using Hanzi
    SitelenMunjan
  deriving stock (Show)

-- | Lojban scripts
data LojbanScripts
  = -- | almost standard latin orthography
    LojbanLatin
  | -- | Zbalermorna abugida
    LojbanZbalermorna
  deriving stock (Show)

-- | Languages
data Language
  = -- | de_DE
    Deutsch
  | -- | en_US, fallback language
    English
  | -- | fr_FR
    Français
  | -- | jbo
    Lojban LojbanScripts
  | -- | nl_NL
    Nederlands
  | -- | tok
    TokiPona TokiPonaScripts
  deriving stock (Show)

parseLanguage :: String -> Language
parseLanguage ('d' : 'e' : _) = Deutsch
parseLanguage ('e' : 'n' : _) = English
parseLanguage ('f' : 'r' : _) = Français
parseLanguage "jbo@ZLR" = Lojban LojbanZbalermorna
parseLanguage ('j' : 'b' : 'o' : _) = Lojban LojbanLatin
parseLanguage ('n' : 'l' : _) = Nederlands
parseLanguage "tok@SP" = TokiPona SitelenPona
parseLanguage "tok@SM" = TokiPona SitelenMunjan
parseLanguage "tok@SE" = TokiPona SitelenEmosi
parseLanguage ('t' : 'o' : 'k' : _) = TokiPona SitelenLasina
parseLanguage _ = English

-- | Text to handler
text :: Text -> WidgetFor a (Either TranslationError Html)
text = pure . Right . toHtml

-- | Unescaped text to handler
html :: Text -> WidgetFor a (Either TranslationError Html)
html = pure . Right . preEscapedToHtml

-- | Untranslated string, falls back to another language
untranslated :: WidgetFor a (Either TranslationError Html)
untranslated = pure $ Left Untranslated