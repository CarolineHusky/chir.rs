{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Text.Internationalisation.EnglishSteno where

import Text.Internationalisation.Types (
  Message (..),
  TranslationError,
  html,
  text,
  untranslated,
 )
import Yesod (Html, WidgetFor)

translate :: Message -> WidgetFor a (Either TranslationError Html)
translate MsgAkkoma = text "A*P/K*/K*/O*/M*/A*"
translate MsgAlternateLink = text "SKWHAFPL/A*LT/SKWHORBG"
translate MsgAndALotMore = text "SKPA/HROT/PHOR/SKWHOEUR"
translate MsgAuthorize = text "THOR"
translate MsgAutomatic = text "AUPL/EUBG"
translate MsgForgejo = text "TPORPBLG/TK-LS/SKWR*/O*"
translate MsgGithub = text "TKPWEUT/HUB"
translate MsgGPGKeyNote = text "SKWHAFPL/G*P/P*P/G*P/KAOE/SKWHORBG"
translate MsgFooterText = text "PHAED/W/KOEF/TPH/HAS/KEL/TP-PL"
translate MsgFootnoteBridgedMatrix = text "BWREUPBLGD/TO/KPA/PHAERBGS/TP-PL"
translate MsgImgSammyTheTanukiBabyLottePFP = text "POR/TRAT/-F/KHARL/OT/KW-BG/ABP/ORPBG/RA/KAOPB/W/P*EUPBG/TPAEUS/SKP/PURPL/PHAFBG/TP-PL/T/S/WAERG/AEU/PURPL/PWAPB/TKAPB/TPHA/ARPBD/EUTS/TPHEBG/SKPA/PURPL/PAS/TPAOEUR/W/AEU/P*EUPBG/HART/ON/TTP-PL"
translate MsgImgSammyTheTanukiBabyLottePFPFigcaption = html "ART/WORBG/PWEU<a href=\"https://furaffinity.net/user/sammythetanuki\" hreflang=\"en\">SAPL/KWREU/-T/T*P/A*/N*/*U/K*/*EU</a>"
translate MsgJumpToContent = text "SKWRUFRP/TO/KAUPBT/SKWHOEUR"
translate MsgMyDiscord = text "STKO*RD/SKWHAFPL/TK*/A*/R*/K*/K*/*EU/R*/PW*/SKWHORBG"
translate MsgMyEmail = text "AOE/PHAEUL/SKWHAFPL/HR*/O*/T*/T*/*E/KWRAT/KR*/H*/EU*/R*/P-P/R*/S*/SKWHORBG"
translate MsgMyFediverse = text "TP*/*E/TK*/*EU/SR*/*E/R*/S*/SKWHAFPL/KWRAT/KR*/H*/A*/R*/HR*/O*/T*/T*/*E/KWRAT/A*/K*/K*/O*/P-P/KR*/H*/I*/R*/P-P/R*/S*/SKWHORBG"
translate MsgMyForgejo = text "PHEU/KPA/TPORPBLG/TK-LS/SKWR*/O*/STPHAPBS"
translate MsgMyKeybase = text "KAOE/TK-LS/PWAEUS/SKWHAFPL/TK*/A*/R*/K*/K*/*EU/R*/PW*/SKWHORBG"
translate MsgMyMatrixChat = text "PHAERBGS/SKWHAFPL/KWRAT/HR*/O*/T*/T*/E*/KHR-PB/KR*/H*/*EU/R*/P-P/R*/S*/SKWHORBG"
translate MsgMySignalWhatsapp = text "STPHAL/OEU/WHAZ/TK-LS/AP/SKWHOLG/SRAEUBL/POPB/KW"
translate MsgMyTelegram = text "TEL/TKPWRAPL/SKWHAFPL/TK*/A*/R*/K*/K*/*EU/R*/PW*/SKWHORBG"
translate MsgNextcloud = text "TPHEGT/TK-LS/KHROUD"
translate MsgPageBlog = text "PWHROG"
translate MsgPageChat = text "KHAT"
translate MsgPageHome = text "HOEPL/PAEUPBLG"
translate MsgPageSubtitle = text "RA/KAOPBZ/H-F/OPBT/STPHET/H-F/T-S/PHOR/HRAOEULG/THAPB/U/THEU/SKHRAPL"
translate MsgPageTitle = text "HROT/TE/AES/AT/TEUBG"
translate MsgPeertube = text "KPA/PAOER/TK-LS/TAOUB"
translate MsgReportedSightings = text "RORPD/SAOEUT/-GS"
translate MsgReportedSightingsSubtitle = text "ORD/-D/PWEU/ORP/KREBLT/TP-PL"
translate MsgSiteIndex = text "TKEUPBGS"
translate MsgSelfIntroPart1 = html "HEU/SKHRAPL/AOEUPL/<span class=\"p-name\">KHARL/OT</span>/KW-BG/OR/SKWRUFT/<span class=\"p-nick\">HROT/TE</span>/KW-BG/AEU/<abbr class=\"dt-bday\" title=\"2001-01-10\">#T-D/KWRAERLD</abbr>/<span class=\"p-species\">RA/KAON</span>/THER/KWRAPB/SKP/<span class=\"p-role\">STAUPB</span>/TPR/<span class=\"p-country-name\">SKWRERPL/TPHEU</span>/TP-PL/"
translate MsgSelfIntroPart2 = text "EU/TKO/S-PL/STKRAEUGS/KW-BG/SWA*EUR/SREPLT/KW-BG/SKP/KWAEULGS/HARD/WAEUR/STKAOEUPB/TP-PL/"
translate MsgSelfIntroPart3 = text "EU/HRUF/KOEF/KW-BG/HRA/SAPB/KWRA/KW-BG/PEUZ/RA/KAONS/KW-BG/SKP/PHRURB/KWREUS/TP-PL/"
translate MsgSelfIntroPart4 = html "MEU/PRO/TPHOUPBZ/R/<span class=\"p-pronoun\">T/OEU/EUTS</span>/KW-BG/<span class=\"p-pronoun\">RA*BG/OEU/RA*BGZ/OEU/RA*BG/SEFL</span>/KW-BG/SKP/<span class=\"p-pronoun\">PHRURB/OEU/PHRURBZ/OEU/PHRURB/SEFL</span>/TP-PL"
translate MsgSettingDarkTheme = text "TKARBG/THAOEPL"
translate MsgSettingLanguage = text "HRAPBLG/STPH-FPLT"
translate MsgSettingLightTheme = text "HRAOEUT/THAOEPL"
translate MsgSettingsCookieNotice = text "WE/AOUS/KAOBG/EUS/TO/STOR/SKP/TREUT/KHAEUPBGD/SEGTS/TP-PL"
translate MsgSettingsMenu = text "SEGTS"
translate MsgSettingTheme = text "THAOEPL/STPH-FPLT"
translate MsgUsername = text "AO*URS/TPHAEUPL/STPH-FPLT"
translate _ = untranslated
