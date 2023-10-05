{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Text.Internationalisation.Deutsch where

import Text.Internationalisation.Types (
  Message (..),
  TranslationError,
  html,
  text,
  untranslated,
 )
import Yesod (Html, WidgetFor)

translate :: Message -> WidgetFor a (Either TranslationError Html)
translate MsgAkkoma = text "Akkoma"
translate MsgAlternateLink = text "(Alt.)"
translate MsgAndALotMore = text "und vieles mehr…"
translate MsgAuthorize = text "Authorisieren"
translate MsgAutomatic = text "Automatisch"
translate MsgGPGKeyNote = text "(GPG-Schlüssel)"
translate MsgFooterText = text "Gemacht mit Kaffee und Haskell."
translate MsgFootnoteBridgedMatrix = text "Mit Matrix verbunden."
translate MsgImgVintagecoyotePrideicon = text "Portrait von Charlotte, ein orangener Waschbär mit rosa Gesicht und lilafarbener Maske. Es lächelt und trägt einen Schal, der mit den Farben der Asexuellen-Pride-Flagge ausgestattet, nämlich schwarz, grau, weiß, und lila. Im hintergrund ist die Therian-Pride-Flagge, welches die Farben grün, weiß, und lila hat, und ein komplexes Symbol mit einem Dreieck, einem Kreis, und einem Heptagramm besteht."
translate MsgImgVintagecoyotePrideiconFigcaption = html "Illustration von <a href=\"https://nitter.net/vintagecoyote\" hreflang=\"en\">yote</a>"
translate MsgJumpToContent = text "Springe zum Inhalt…"
translate MsgMyFediverse = text "Fediversum (@charlotte@akko.chir.rs)"
translate MsgMyForgejo = text "Mein Forgejo Server"
translate MsgMySignalWhatsapp = text "Signal/Whatsapp: verfügbar auf Nachfrage"
translate MsgPageSubtitle = text "Waschbären? Im Internet? Das ist wahrscheinlicher als es klingt!"
translate MsgPageTitle = text "Lottes Dachboden"
translate MsgReportedSightings = text "Gemeldete Sichtungen"
translate MsgReportedSightingsSubtitle = text "Sortiert nach Meldungsseriösität."
translate MsgSelfIntroPart1 = html "Hi! Ich bin <span class=\"p-name\">Charlotte</span>, oder auch nur <span class=\"p-nick\">Lotte</span>, eine <abbr class=\"dt-bday\" title=\"2001-01-10\">22 Jahre alte</abbr> Waschbärin-Therian und <span class=\"p-role\">Studentin</span> aus <span class=\"p-country-name\">Deutschland</span>."
translate MsgSelfIntroPart2 = text "Ich bin Systemadministratorin, Softwareentwicklerin, und manchmal Hardwaredesignerin."
translate MsgSelfIntroPart3 = text "Ich liebe Kaffe, Lasagne, Pizza, Waschbären, und Plüschtiere."
translate MsgSelfIntroPart4 = html "Meine Pronomen sind <span class=\"p-pronoun\">es</span> und <span class=\"-pronoun\">sie</span>."
translate MsgSettingDarkTheme = text "Dunkler Stil"
translate MsgSettingLanguage = text "Sprache:"
translate MsgSettingLightTheme = text "Heller Stil"
translate MsgSettingsCookieNotice = text "Wir benutzen Cookies, um veränderte Einstellungen zu speichern und an dem Server zu senden."
translate MsgSettingsMenu = text "Einstellungen"
translate MsgSettingTheme = text "Stil:"
translate MsgUsername = text "Nutzername:"
translate _ = untranslated
