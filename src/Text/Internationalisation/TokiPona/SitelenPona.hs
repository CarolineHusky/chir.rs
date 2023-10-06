module Text.Internationalisation.TokiPona.SitelenPona where

import Text.PUA.TH (includePUAHS)
import Utils (intersperseAfter)

includePUAHS "src/Text/Internationalisation/TokiPona/SitelenPona.phs"

cartouche :: String -> Text
cartouche t = toText $ '\xF1990' : intersperseAfter '\xF1992' (toCartouche t) ++ "\xF1991"

fromSitelenPona :: String -> String
fromSitelenPona = id