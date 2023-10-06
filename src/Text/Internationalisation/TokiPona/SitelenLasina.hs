module Text.Internationalisation.TokiPona.SitelenLasina where

import Text.Internationalisation.TokiPona.Common (spToString)
import Utils (capitalize)

cartouche :: String -> Text
cartouche = toText . capitalize

fromSitelenPona :: String -> String
fromSitelenPona = spToString True logogramToText

logogramToText :: Char -> String
logogramToText '\x0F1900' = "a"
logogramToText '\x0F1901' = "akesi"
logogramToText '\x0F1902' = "ala"
logogramToText '\x0F1903' = "alasa"
logogramToText '\x0F1904' = "ale"
logogramToText '\x0F1905' = "anpa"
logogramToText '\x0F1906' = "ante"
logogramToText '\x0F1907' = "anu"
logogramToText '\x0F1908' = "awen"
logogramToText '\x0F1909' = "e"
logogramToText '\x0F190A' = "en"
logogramToText '\x0F190B' = "esun"
logogramToText '\x0F190C' = "ijo"
logogramToText '\x0F190D' = "ike"
logogramToText '\x0F190E' = "ilo"
logogramToText '\x0F190F' = "insa"
logogramToText '\x0F1910' = "jaki"
logogramToText '\x0F1911' = "jan"
logogramToText '\x0F1912' = "jelo"
logogramToText '\x0F1913' = "jo"
logogramToText '\x0F1914' = "kala"
logogramToText '\x0F1915' = "kalama"
logogramToText '\x0F1916' = "kama"
logogramToText '\x0F1917' = "kasi"
logogramToText '\x0F1918' = "ken"
logogramToText '\x0F1919' = "kepeken"
logogramToText '\x0F191A' = "kili"
logogramToText '\x0F191B' = "kiwen"
logogramToText '\x0F191C' = "ko"
logogramToText '\x0F191D' = "kon"
logogramToText '\x0F191E' = "kule"
logogramToText '\x0F191F' = "kulupu"
logogramToText '\x0F1920' = "kute"
logogramToText '\x0F1921' = "la"
logogramToText '\x0F1922' = "lape"
logogramToText '\x0F1923' = "laso"
logogramToText '\x0F1924' = "lawa"
logogramToText '\x0F1925' = "len"
logogramToText '\x0F1926' = "lete"
logogramToText '\x0F1927' = "li"
logogramToText '\x0F1928' = "lili"
logogramToText '\x0F1929' = "linja"
logogramToText '\x0F192A' = "lipu"
logogramToText '\x0F192B' = "loje"
logogramToText '\x0F192C' = "lon"
logogramToText '\x0F192D' = "luka"
logogramToText '\x0F192E' = "lukin"
logogramToText '\x0F192F' = "lupa"
logogramToText '\x0F1930' = "ma"
logogramToText '\x0F1931' = "mama"
logogramToText '\x0F1932' = "mani"
logogramToText '\x0F1933' = "meli"
logogramToText '\x0F1934' = "mi"
logogramToText '\x0F1935' = "mije"
logogramToText '\x0F1936' = "moku"
logogramToText '\x0F1937' = "moli"
logogramToText '\x0F1938' = "monsi"
logogramToText '\x0F1939' = "mu"
logogramToText '\x0F193A' = "mun"
logogramToText '\x0F193B' = "musi"
logogramToText '\x0F193C' = "mute"
logogramToText '\x0F193D' = "nanpa"
logogramToText '\x0F193E' = "nasa"
logogramToText '\x0F193F' = "nasin"
logogramToText '\x0F1940' = "nena"
logogramToText '\x0F1941' = "ni"
logogramToText '\x0F1942' = "nimi"
logogramToText '\x0F1943' = "noka"
logogramToText '\x0F1944' = "o"
logogramToText '\x0F1945' = "olin"
logogramToText '\x0F1946' = "ona"
logogramToText '\x0F1947' = "open"
logogramToText '\x0F1948' = "pakala"
logogramToText '\x0F1949' = "pali"
logogramToText '\x0F194A' = "palisa"
logogramToText '\x0F194B' = "pan"
logogramToText '\x0F194C' = "pana"
logogramToText '\x0F194D' = "pi"
logogramToText '\x0F194E' = "pilin"
logogramToText '\x0F194F' = "pimeja"
logogramToText '\x0F1950' = "pini"
logogramToText '\x0F1951' = "pipi"
logogramToText '\x0F1952' = "poka"
logogramToText '\x0F1953' = "poki"
logogramToText '\x0F1954' = "pona"
logogramToText '\x0F1955' = "pu"
logogramToText '\x0F1956' = "sama"
logogramToText '\x0F1957' = "seli"
logogramToText '\x0F1958' = "selo"
logogramToText '\x0F1959' = "seme"
logogramToText '\x0F195A' = "sewi"
logogramToText '\x0F195B' = "sijelo"
logogramToText '\x0F195C' = "sike"
logogramToText '\x0F195D' = "sin"
logogramToText '\x0F195E' = "sina"
logogramToText '\x0F195F' = "sinpin"
logogramToText '\x0F1960' = "sitelen"
logogramToText '\x0F1961' = "sona"
logogramToText '\x0F1962' = "soweli"
logogramToText '\x0F1963' = "suli"
logogramToText '\x0F1964' = "suno"
logogramToText '\x0F1965' = "supa"
logogramToText '\x0F1966' = "suwi"
logogramToText '\x0F1967' = "tan"
logogramToText '\x0F1968' = "taso"
logogramToText '\x0F1969' = "tawa"
logogramToText '\x0F196A' = "telo"
logogramToText '\x0F196B' = "tenpo"
logogramToText '\x0F196C' = "toki"
logogramToText '\x0F196D' = "tomo"
logogramToText '\x0F196E' = "tu"
logogramToText '\x0F196F' = "unpa"
logogramToText '\x0F1970' = "uta"
logogramToText '\x0F1971' = "utala"
logogramToText '\x0F1972' = "walo"
logogramToText '\x0F1973' = "wan"
logogramToText '\x0F1974' = "waso"
logogramToText '\x0F1975' = "wawa"
logogramToText '\x0F1976' = "weka"
logogramToText '\x0F1977' = "wile"
logogramToText '\x0F1978' = "namako"
logogramToText '\x0F1979' = "kin"
logogramToText '\x0F197A' = "oko"
logogramToText '\x0F197B' = "kipisi"
logogramToText '\x0F197C' = "leko"
logogramToText '\x0F197D' = "monsuta"
logogramToText '\x0F197E' = "tonsi"
logogramToText '\x0F197F' = "jasima"
logogramToText '\x0F1980' = "kijetesantakalu"
logogramToText '\x0F1981' = "soko"
logogramToText '\x0F1982' = "meso"
logogramToText '\x0F1983' = "epiku"
logogramToText '\x0F1984' = "kokosila"
logogramToText '\x0F1985' = "lanpan"
logogramToText '\x0F1986' = "n"
logogramToText '\x0F1987' = "misikeke"
logogramToText '\x0F1988' = "ku"
logogramToText '\x0F19A0' = "pake"
logogramToText '\x0F19A1' = "apeja"
logogramToText '\x0F19A2' = "majuna"
logogramToText '\x0F19A3' = "powe"
logogramToText '\x0F1993' = "pi"
logogramToText '\xff900' = "anpa lawa"
logogramToText '\xff901' = "ijo akesi"
logogramToText '\xff902' = "ijo ala"
logogramToText '\xff903' = "ijo alasa"
logogramToText '\xff904' = "ijo ale"
logogramToText '\xff905' = "ijo anpa"
logogramToText '\xff906' = "ijo ante"
logogramToText '\xff907' = "ijo anu"
logogramToText '\xff908' = "ijo apeja"
logogramToText '\xff909' = "ijo awen"
logogramToText '\xff90a' = "ijo en"
logogramToText '\xff90b' = "ijo esun"
logogramToText '\xff90c' = "ijo ike"
logogramToText '\xff90d' = "ijo ilo"
logogramToText '\xff90e' = "ijo insa"
logogramToText '\xff90f' = "ijo jaki"
logogramToText '\xff910' = "ijo jan"
logogramToText '\xff911' = "ijo jelo"
logogramToText '\xff912' = "ijo jo"
logogramToText '\xff913' = "ijo kala"
logogramToText '\xff914' = "ijo kalama"
logogramToText '\xff915' = "ijo kama"
logogramToText '\xff916' = "ijo kasi"
logogramToText '\xff917' = "ijo ken"
logogramToText '\xff918' = "ijo kepeken"
logogramToText '\xff919' = "ijo kijetesantakalu"
logogramToText '\xff91a' = "ijo kili"
logogramToText '\xff91b' = "ijo kin"
logogramToText '\xff91c' = "ijo kipisi"
logogramToText '\xff91d' = "ijo kiwen"
logogramToText '\xff91e' = "ijo ko"
logogramToText '\xff91f' = "ijo kon"
logogramToText '\xff920' = "ijo kule"
logogramToText '\xff921' = "ijo kulupu"
logogramToText '\xff922' = "ijo kute"
logogramToText '\xff923' = "ijo lape"
logogramToText '\xff924' = "ijo laso"
logogramToText '\xff925' = "ijo lawa"
logogramToText '\xff926' = "ijo leko"
logogramToText '\xff927' = "ijo len"
logogramToText '\xff928' = "ijo lete"
logogramToText '\xff929' = "ijo lili"
logogramToText '\xff92a' = "ijo linja"
logogramToText '\xff92b' = "ijo lipu"
logogramToText '\xff92c' = "ijo loje"
logogramToText '\xff92d' = "ijo lon"
logogramToText '\xff92e' = "ijo luka"
logogramToText '\xff92f' = "ijo lukin"
logogramToText '\xff930' = "ijo lupa"
logogramToText '\xff931' = "ijo ma"
logogramToText '\xff932' = "ijo mama"
logogramToText '\xff933' = "ijo mani"
logogramToText '\xff934' = "ijo meli"
logogramToText '\xff935' = "ijo mi"
logogramToText '\xff936' = "ijo mije"
logogramToText '\xff937' = "ijo moku"
logogramToText '\xff938' = "ijo moli"
logogramToText '\xff939' = "ijo monsi"
logogramToText '\xff93a' = "ijo monsuta"
logogramToText '\xff93b' = "ijo mu"
logogramToText '\xff93c' = "ijo mun"
logogramToText '\xff93d' = "ijo musi"
logogramToText '\xff93e' = "ijo mute"
logogramToText '\xff93f' = "ijo namako"
logogramToText '\xff940' = "ijo nanpa"
logogramToText '\xff941' = "ijo nasa"
logogramToText '\xff942' = "ijo nasin"
logogramToText '\xff943' = "ijo nena"
logogramToText '\xff944' = "ijo ni"
logogramToText '\xff945' = "ijo nimi"
logogramToText '\xff946' = "ijo noka"
logogramToText '\xff947' = "ijo oko"
logogramToText '\xff948' = "ijo olin"
logogramToText '\xff949' = "ijo ona"
logogramToText '\xff94a' = "ijo open"
logogramToText '\xff94b' = "ijo pakala"
logogramToText '\xff94c' = "ijo pake"
logogramToText '\xff94d' = "ijo pali"
logogramToText '\xff94e' = "ijo palisa"
logogramToText '\xff94f' = "ijo pan"
logogramToText '\xff950' = "ijo pana"
logogramToText '\xff951' = "ijo pilin"
logogramToText '\xff952' = "ijo pimeja"
logogramToText '\xff953' = "ijo pini"
logogramToText '\xff954' = "ijo pipi"
logogramToText '\xff955' = "ijo poka"
logogramToText '\xff956' = "ijo poki"
logogramToText '\xff957' = "ijo pona"
logogramToText '\xff958' = "ijo pu"
logogramToText '\xff959' = "ijo sama"
logogramToText '\xff95a' = "ijo seli"
logogramToText '\xff95b' = "ijo selo"
logogramToText '\xff95c' = "ijo seme"
logogramToText '\xff95d' = "ijo sewi"
logogramToText '\xff95e' = "ijo sijelo"
logogramToText '\xff95f' = "ijo sike"
logogramToText '\xff960' = "ijo sin"
logogramToText '\xff961' = "ijo sina"
logogramToText '\xff962' = "ijo sinpin"
logogramToText '\xff963' = "ijo sitelen"
logogramToText '\xff964' = "ijo sona"
logogramToText '\xff965' = "ijo soweli"
logogramToText '\xff966' = "ijo suli"
logogramToText '\xff967' = "ijo suno"
logogramToText '\xff968' = "ijo supa"
logogramToText '\xff969' = "ijo suwi"
logogramToText '\xff96a' = "ijo tan"
logogramToText '\xff96b' = "ijo taso"
logogramToText '\xff96c' = "ijo tawa"
logogramToText '\xff96d' = "ijo telo"
logogramToText '\xff96e' = "ijo tenpo"
logogramToText '\xff96f' = "ijo toki"
logogramToText '\xff970' = "ijo tomo"
logogramToText '\xff971' = "ijo tonsi"
logogramToText '\xff972' = "ijo tu"
logogramToText '\xff973' = "ijo unpa"
logogramToText '\xff974' = "ijo uta"
logogramToText '\xff975' = "ijo utala"
logogramToText '\xff976' = "ijo walo"
logogramToText '\xff977' = "ijo wan"
logogramToText '\xff978' = "ijo waso"
logogramToText '\xff979' = "ijo wawa"
logogramToText '\xff97a' = "ijo weka"
logogramToText '\xff97b' = "ijo wile"
logogramToText '\xff97c' = "ike ala"
logogramToText '\xff97d' = "ike lili"
logogramToText '\xff97e' = "ike lukin"
logogramToText '\xff97f' = "ilo kipisi"
logogramToText '\xff980' = "ilo lape"
logogramToText '\xff981' = "ilo lukin"
logogramToText '\xff982' = "ilo moli"
logogramToText '\xff983' = "ilo musi"
logogramToText '\xff984' = "ilo nanpa"
logogramToText '\xff985' = "ilo oko"
logogramToText '\xff986' = "ilo open"
logogramToText '\xff987' = "ilo suno"
logogramToText '\xff988' = "ilo toki"
logogramToText '\xff989' = "jan ala"
logogramToText '\xff98a' = "jan alasa"
logogramToText '\xff98b' = "jan ale"
logogramToText '\xff98c' = "jan ante"
logogramToText '\xff98d' = "jan ike"
logogramToText '\xff98e' = "jan kala"
logogramToText '\xff98f' = "jan kalama"
logogramToText '\xff990' = "jan kasi"
logogramToText '\xff991' = "jan kulupu"
logogramToText '\xff992' = "jan lawa"
logogramToText '\xff993' = "jan lili"
logogramToText '\xff994' = "jan monsuta"
logogramToText '\xff995' = "jan mute"
logogramToText '\xff996' = "jan nasa"
logogramToText '\xff997' = "jan olin"
logogramToText '\xff998' = "jan pakala"
logogramToText '\xff999' = "jan pali"
logogramToText '\xff99a' = "jan poka"
logogramToText '\xff99b' = "jan pona"
logogramToText '\xff99c' = "jan sama"
logogramToText '\xff99d' = "jan seme"
logogramToText '\xff99e' = "jan sewi"
logogramToText '\xff99f' = "jan sin"
logogramToText '\xff9a0' = "jan sona"
logogramToText '\xff9a1' = "jan suli"
logogramToText '\xff9a2' = "jan suwi"
logogramToText '\xff9a3' = "jan toki"
logogramToText '\xff9a4' = "jan unpa"
logogramToText '\xff9a5' = "jan utala"
logogramToText '\xff9a6' = "jan wawa"
logogramToText '\xff9a7' = "kala lete"
logogramToText '\xff9a8' = "kala lili"
logogramToText '\xff9a9' = "kalama musi"
logogramToText '\xff9aa' = "kasi jelo"
logogramToText '\xff9ab' = "kasi kule"
logogramToText '\xff9ac' = "kasi laso"
logogramToText '\xff9ad' = "kasi lili"
logogramToText '\xff9ae' = "kasi loje"
logogramToText '\xff9af' = "kasi pimeja"
logogramToText '\xff9b0' = "kasi walo"
logogramToText '\xff9b1' = "kili jelo"
logogramToText '\xff9b2' = "kili laso"
logogramToText '\xff9b3' = "kili lili"
logogramToText '\xff9b4' = "kili loje"
logogramToText '\xff9b5' = "kili palisa"
logogramToText '\xff9b6' = "kili pimeja"
logogramToText '\xff9b7' = "kili suwi"
logogramToText '\xff9b8' = "kili walo"
logogramToText '\xff9b9' = "kiwen jelo"
logogramToText '\xff9ba' = "kiwen kasi"
logogramToText '\xff9bb' = "kiwen laso"
logogramToText '\xff9bc' = "kiwen lete"
logogramToText '\xff9bd' = "kiwen lili"
logogramToText '\xff9be' = "kiwen loje"
logogramToText '\xff9bf' = "kiwen mun"
logogramToText '\xff9c0' = "kiwen pimeja"
logogramToText '\xff9c1' = "kiwen seli"
logogramToText '\xff9c2' = "kiwen suno"
logogramToText '\xff9c3' = "kiwen walo"
logogramToText '\xff9c4' = "ko jaki"
logogramToText '\xff9c5' = "ko jelo"
logogramToText '\xff9c6' = "ko kasi"
logogramToText '\xff9c7' = "ko kule"
logogramToText '\xff9c8' = "ko laso"
logogramToText '\xff9c9' = "ko lete"
logogramToText '\xff9ca' = "ko lili"
logogramToText '\xff9cb' = "ko loje"
logogramToText '\xff9cc' = "ko nasa"
logogramToText '\xff9cd' = "ko pimeja"
logogramToText '\xff9ce' = "ko seli"
logogramToText '\xff9cf' = "ko walo"
logogramToText '\xff9d0' = "kon lete"
logogramToText '\xff9d1' = "len jan"
logogramToText '\xff9d2' = "len jelo"
logogramToText '\xff9d3' = "len laso"
logogramToText '\xff9d4' = "len lawa"
logogramToText '\xff9d5' = "len lili"
logogramToText '\xff9d6' = "len loje"
logogramToText '\xff9d7' = "len luka"
logogramToText '\xff9d8' = "len noka"
logogramToText '\xff9d9' = "len pimeja"
logogramToText '\xff9da' = "len sin"
logogramToText '\xff9db' = "len walo"
logogramToText '\xff9dc' = "linja lili"
logogramToText '\xff9dd' = "linja pona"
logogramToText '\xff9de' = "lipu kasi"
logogramToText '\xff9df' = "lipu nanpa"
logogramToText '\xff9e0' = "lipu sewi"
logogramToText '\xff9e1' = "lipu sona"
logogramToText '\xff9e2' = "lipu toki"
logogramToText '\xff9e3' = "luka luka"
logogramToText '\xff9e4' = "lupa jaki"
logogramToText '\xff9e5' = "lupa kiwen"
logogramToText '\xff9e6' = "lupa kute"
logogramToText '\xff9e7' = "lupa lili"
logogramToText '\xff9e8' = "lupa meli"
logogramToText '\xff9e9' = "lupa monsi"
logogramToText '\xff9ea' = "lupa nena"
logogramToText '\xff9eb' = "lupa tomo"
logogramToText '\xff9ec' = "ma ale"
logogramToText '\xff9ed' = "ma kasi"
logogramToText '\xff9ee' = "ma tomo"
logogramToText '\xff9ef' = "mama mama"
logogramToText '\xff9f0' = "mama meli"
logogramToText '\xff9f1' = "mama mije"
logogramToText '\xff9f2' = "meli ike"
logogramToText '\xff9f3' = "meli lili"
logogramToText '\xff9f4' = "meli pona"
logogramToText '\xff9f5' = "meli sama"
logogramToText '\xff9f6' = "meli unpa"
logogramToText '\xff9f7' = "mije ike"
logogramToText '\xff9f8' = "mije lili"
logogramToText '\xff9f9' = "mije pona"
logogramToText '\xff9fa' = "mije sama"
logogramToText '\xff9fb' = "mije unpa"
logogramToText '\xff9fc' = "mije wawa"
logogramToText '\xff9fd' = "musi lili"
logogramToText '\xff9fe' = "nena kon"
logogramToText '\xff9ff' = "nena kute"
logogramToText '\xffa00' = "nena lili"
logogramToText '\xffa01' = "nena mama"
logogramToText '\xffa02' = "nena meli"
logogramToText '\xffa03' = "palisa lili"
logogramToText '\xffa04' = "pilin ala"
logogramToText '\xffa05' = "pilin ike"
logogramToText '\xffa06' = "pilin nasa"
logogramToText '\xffa07' = "pilin pakala"
logogramToText '\xffa08' = "pilin pona"
logogramToText '\xffa09' = "pilin sama"
logogramToText '\xffa0a' = "poki kon"
logogramToText '\xffa0b' = "poki len"
logogramToText '\xffa0c' = "poki lete"
logogramToText '\xffa0d' = "poki lili"
logogramToText '\xffa0e' = "poki seli"
logogramToText '\xffa0f' = "poki telo"
logogramToText '\xffa10' = "pona ala"
logogramToText '\xffa11' = "pona lili"
logogramToText '\xffa12' = "pona lukin"
logogramToText '\xffa13' = "selo len"
logogramToText '\xffa14' = "selo soweli"
logogramToText '\xffa15' = "sike lili"
logogramToText '\xffa16' = "sitelen ike"
logogramToText '\xffa17' = "sitelen ma"
logogramToText '\xffa18' = "sitelen monsuta"
logogramToText '\xffa19' = "sitelen pona"
logogramToText '\xffa1a' = "sitelen sitelen"
logogramToText '\xffa1b' = "sitelen tawa"
logogramToText '\xffa1c' = "sitelen toki"
logogramToText '\xffa1d' = "sona ala"
logogramToText '\xffa1e' = "sona ike"
logogramToText '\xffa1f' = "sona lili"
logogramToText '\xffa20' = "sona ma"
logogramToText '\xffa21' = "sona nanpa"
logogramToText '\xffa22' = "sona pona"
logogramToText '\xffa23' = "sona sijelo"
logogramToText '\xffa24' = "sona tenpo"
logogramToText '\xffa25' = "sona toki"
logogramToText '\xffa26' = "sona utala"
logogramToText '\xffa27' = "supa lape"
logogramToText '\xffa28' = "supa lawa"
logogramToText '\xffa29' = "supa lupa"
logogramToText '\xffa2a' = "supa moku"
logogramToText '\xffa2b' = "supa monsi"
logogramToText '\xffa2c' = "supa pali"
logogramToText '\xffa2d' = "telo lete"
logogramToText '\xffa2e' = "telo lili"
logogramToText '\xffa2f' = "toki ala"
logogramToText '\xffa30' = "toki ike"
logogramToText '\xffa31' = "toki pona"
logogramToText '\xffa32' = "toki sin"
logogramToText '\xffa33' = "toki sona"
logogramToText '\xffa34' = "toki utala"
logogramToText '\xffa35' = "tomo lape"
logogramToText '\xffa36' = "tomo mani"
logogramToText '\xffa37' = "tomo moku"
logogramToText '\xffa38' = "tomo monsuta"
logogramToText '\xffa39' = "tomo nasin"
logogramToText '\xffa3a' = "tomo pali"
logogramToText '\xffa3b' = "tomo sona"
logogramToText '\xffa3c' = "tomo tawa"
logogramToText '\xffa3d' = "tomo unpa"
logogramToText '\xffa3e' = "tomo utala"
logogramToText '\xffa3f' = "tu tu"
logogramToText '\xffa40' = "tu wan"
logogramToText '\xffa41' = "wan tu"
logogramToText '\xffa42' = "luka wan"
logogramToText '\xffa43' = "wan luka"
logogramToText '\xffa44' = "luka tu"
logogramToText '\xffa45' = "tu luka"
logogramToText '\xffa46' = "luka tu wan"
logogramToText '\xffa47' = "luka wan tu"
logogramToText '\xffa48' = "luka tu tu"
logogramToText '\xffa49' = "luka luka"
logogramToText '\xffa4a' = "luka luka wan"
logogramToText '\xffa4b' = "luka luka tu"
logogramToText '\xffa4c' = "luka luka tu wan"
logogramToText '\xffa4d' = "luka luka wan tu"
logogramToText '\xffa4e' = "luka luka tu tu"
logogramToText '\xffa4f' = "luka luka luka"
logogramToText '\xffa50' = ""
logogramToText '\xffa51' = ""
logogramToText '\xffa52' = "a"
logogramToText '\xffa53' = "ala"
logogramToText '\xffa54' = "ala"
logogramToText '\xffa55' = "ala"
logogramToText '\xffa56' = "awen"
logogramToText '\xffa57' = "awen"
logogramToText '\xffa58' = "awen"
logogramToText '\xffa59' = "esun"
logogramToText '\xffa5a' = "kama"
logogramToText '\xffa5b' = "ken"
logogramToText '\xffa5c' = "ken"
logogramToText '\xffa5d' = "ken"
logogramToText '\xffa5e' = "kepeken"
logogramToText '\xffa5f' = "len"
logogramToText '\xffa60' = "len"
logogramToText '\xffa61' = "len"
logogramToText '\xffa62' = "linja"
logogramToText '\xffa63' = "lon"
logogramToText '\xffa64' = "lon"
logogramToText '\xffa65' = "lon"
logogramToText '\xffa66' = "luka"
logogramToText '\xffa67' = "meli"
logogramToText '\xffa68' = "meli"
logogramToText '\xffa69' = "meli"
logogramToText '\xffa6a' = "mi"
logogramToText '\xffa6b' = "mije"
logogramToText '\xffa6c' = "mije"
logogramToText '\xffa6d' = "mije"
logogramToText '\xffa6e' = "moku"
logogramToText '\xffa6f' = "monsi"
logogramToText '\xffa70' = "mute"
logogramToText '\xffa71' = "mute"
logogramToText '\xffa72' = "mute"
logogramToText '\xffa73' = "nanpa"
logogramToText '\xffa74' = "nanpa"
logogramToText '\xffa75' = "nanpa"
logogramToText '\xffa76' = "nena"
logogramToText '\xffa77' = "nena"
logogramToText '\xffa78' = "nena"
logogramToText '\xffa79' = "ona"
logogramToText '\xffa7a' = "pali"
logogramToText '\xffa7b' = "pana"
logogramToText '\xffa7c' = "pi"
logogramToText '\xffa7d' = "pini"
logogramToText '\xffa7e' = "pini"
logogramToText '\xffa7f' = "pini"
logogramToText '\xffa80' = "selo"
logogramToText '\xffa81' = "selo"
logogramToText '\xffa82' = "selo"
logogramToText '\xffa83' = "sewi"
logogramToText '\xffa84' = "sijelo"
logogramToText '\xffa85' = "sijelo"
logogramToText '\xffa86' = "sijelo"
logogramToText '\xffa87' = "sinpin"
logogramToText '\xffa88' = "soweli"
logogramToText '\xffa89' = "soweli"
logogramToText '\xffa8a' = "soweli"
logogramToText '\xffa8b' = "supa"
logogramToText '\xffa8c' = "supa"
logogramToText '\xffa8d' = "supa"
logogramToText '\xffa8e' = "taso"
logogramToText '\xffa8f' = "tawa"
logogramToText '\xffa90' = "telo"
logogramToText '\xffa91' = "waso"
logogramToText '\xffa92' = "weka"
logogramToText '\xffa93' = "weka"
logogramToText '\xffa94' = "weka"
logogramToText '\xffa95' = "oko"
logogramToText '\xffa96' = "kipisi"
logogramToText '\xffa97' = "jasima"
logogramToText '\xffa98' = "kijetesantakalu"
logogramToText '\xffa99' = "meso"
logogramToText '\xffa9a' = "meso"
logogramToText '\xffa9b' = "meso"
logogramToText '\xffa9c' = "lanpan"
logogramToText '\xffa9d' = "n"
logogramToText '\xffa9e' = "n"
logogramToText '\xffa9f' = "n"
logogramToText '\xffaa0' = "kala"
logogramToText '\xffaa1' = "meli"
logogramToText '\xffaa2' = "mije"
logogramToText '\xffaa3' = "olin"
logogramToText '\xffaa4' = "sewi"
logogramToText '\xffaa5' = "uta"
logogramToText '\xffaa6' = "wile"
logogramToText '\xffaa7' = "namako"
logogramToText '\xffaa8' = "namako"
logogramToText '\xffaa9' = "meso"
logogramToText '\xffaaa' = "meso"
logogramToText '\xffaab' = "epiku"
logogramToText '\xffaac' = "epiku"
logogramToText '\xffaad' = "kokosila"
logogramToText '\xffaae' = "kokosila"
logogramToText '\xffaaf' = "lanpan"
logogramToText '\xffab0' = "lanpan"
logogramToText '\xffab1' = "lanpan"
logogramToText '\xffab2' = "majuna"
logogramToText '\xffab3' = "majuna"
logogramToText '\xffab4' = "sewi"
logogramToText '\xffab5' = "sewi"
logogramToText '\xffab6' = "sewi"
logogramToText '\xffab7' = "lanpan"
logogramToText '\xffab8' = ""
logogramToText '\xffab9' = ""
logogramToText '\xffaba' = ""
logogramToText '\xffabb' = "kijetesantakalu"
logogramToText '\xffabc' = "kijetesantakalu"
logogramToText '\xffabd' = "nata"
logogramToText '\xffabe' = "lipamanka"
logogramToText '\xffabf' = "melinjakulekule"
logogramToText '\xffac0' = "isipin"
logogramToText '\xffac1' = "kapesi"
logogramToText '\xffac2' = "kapesi"
logogramToText '\xffac3' = "kiki"
logogramToText '\xffac4' = "linluwi"
logogramToText '\xffac5' = "linluwi"
logogramToText '\xffac6' = "linluwi"
logogramToText '\xffac7' = "linluwi"
logogramToText '\xffac8' = "mulapisu"
logogramToText '\xffac9' = "samu"
logogramToText '\xffaca' = "unu"
logogramToText '\xffacb' = "wa"
logogramToText '\xffacc' = "ete"
logogramToText '\xffacd' = "ete"
logogramToText '\xfface' = "ete"
logogramToText '\xffacf' = "kan"
logogramToText '\xffad0' = "kuntu"
logogramToText '\xffad1' = "loka"
logogramToText '\xffad2' = "misa"
logogramToText '\xffad3' = "misa"
logogramToText '\xffad4' = "misa"
logogramToText '\xffad5' = "misa"
logogramToText '\xffad6' = "misa"
logogramToText '\xffad7' = "misa"
logogramToText '\xffad8' = "waleja"
logogramToText '\xffad9' = "ewe"
logogramToText '\xffada' = "kamalawala"
logogramToText '\xffadb' = "ke"
logogramToText '\xffadc' = "ke"
logogramToText '\xffadd' = "ke"
logogramToText '\xffade' = "ke"
logogramToText '\xffadf' = "kese"
logogramToText '\xffae0' = "kese"
logogramToText '\xffae1' = "kulijo"
logogramToText '\xffae2' = "likujo"
logogramToText '\xffae3' = "neja"
logogramToText '\xffae4' = "oke"
logogramToText '\xffae5' = "pata"
logogramToText '\xffae6' = "peto"
logogramToText '\xffae7' = "po"
logogramToText '\xffae8' = "polinpin"
logogramToText '\xffae9' = "pomotolo"
logogramToText '\xffaea' = "pomotolo"
logogramToText '\xffaeb' = "san"
logogramToText '\xffaec' = "soto"
logogramToText '\xffaed' = "taki"
logogramToText '\xffaee' = "taki"
logogramToText '\xffaef' = "teje"
logogramToText '\xffaf0' = "te"
logogramToText '\xffaf1' = "to"
logogramToText '\xffaf2' = "umesu"
logogramToText '\xffaf3' = "umesu"
logogramToText '\xffaf4' = "umesu"
logogramToText '\xffaf5' = "umesu"
logogramToText '\xffaf6' = "usawi"
logogramToText '\xffaf7' = "wasoweli"
logogramToText '\xffaf8' = "wasoweli"
logogramToText '\xffaf9' = "wasoweli"
logogramToText '\xffafa' = "wasoweli"
logogramToText '\xffafb' = "sutopatikuna"
logogramToText '\xffafc' = "yupekosi"
logogramToText '\xffafd' = "pingo"
logogramToText '\xffafe' = "kalamarr"
logogramToText '\xffaff' = "akesi"
logogramToText '\xffb00' = "ni"
logogramToText '\xffb01' = "ni"
logogramToText '\xffb02' = "ni"
logogramToText '\xffb03' = "ni"
logogramToText '\xffb04' = "mi"
logogramToText '\xffb05' = "mi"
logogramToText '\xffb06' = "sina"
logogramToText '\xffb07' = "sina"
logogramToText '\xffb08' = "ona"
logogramToText '\xffb09' = "ona"
logogramToText '\xffb0a' = "iki"
logogramToText '\xffb0b' = "iki"
logogramToText '\xffb0c' = "ipi"
logogramToText '\xffb0d' = "ipi"
logogramToText '\xffb0e' = "wi"
logogramToText '\xffb0f' = "wi"
logogramToText '\xffb10' = "a"
logogramToText '\xffb11' = "a"
logogramToText '\xffb12' = "o"
logogramToText '\xffb13' = "o"
logogramToText '\xffb14' = "kin"
logogramToText '\xffb15' = "kin"
logogramToText '\xffb16' = "n"
logogramToText '\xffb17' = "n"
logogramToText '\xffb18' = "n"
logogramToText '\xffb19' = "n"
logogramToText '\xffb1a' = "n"
logogramToText '\xffb1b' = "n"
logogramToText '\xffb1c' = "n"
logogramToText '\xffb1d' = "n"
logogramToText '\xffb1e' = "wa"
logogramToText '\xffb1f' = "wa"
logogramToText '\xffb20' = "ke"
logogramToText '\xffb21' = "ke"
logogramToText '\xffb22' = "ke"
logogramToText '\xffb23' = "ke"
logogramToText '\xffb24' = "ke"
logogramToText '\xffb25' = "ke"
logogramToText '\xffb26' = "ke"
logogramToText '\xffb27' = "ke"
logogramToText '\xffb28' = "je"
logogramToText '\xffb29' = "je"
logogramToText '\xffb2a' = "ako"
logogramToText '\xffb2b' = "ako"
logogramToText '\xffb2c' = "ju"
logogramToText '\xffb2d' = "lu"
logogramToText '\xffb2e' = "nu"
logogramToText '\xffb2f' = "su"
logogramToText '\xffb30' = "u"
logogramToText '\xffb31' = "jami"
logogramToText '\xffb32' = "melome"
logogramToText '\xffb33' = "melome"
logogramToText '\xffb34' = "melome"
logogramToText '\xffb35' = "melome"
logogramToText '\xffb36' = "mijomi"
logogramToText '\xffb37' = "mijomi"
logogramToText '\xffb38' = "mijomi"
logogramToText '\xffb39' = "mijomi"
logogramToText '\xffb3a' = "natu"
logogramToText '\xffb3b' = "omekapo"
logogramToText '\xffb3c' = "puwa"
logogramToText '\xffb3d' = "su"
logogramToText '\xffb3e' = "wekama"
logogramToText '\xffb3f' = "wekama"
logogramToText '\xffb40' = "alu"
logogramToText '\xffb41' = "awase"
logogramToText '\xffb42' = "awase"
logogramToText '\xffb43' = "enko"
logogramToText '\xffb44' = "enko"
logogramToText '\xffb45' = "enko"
logogramToText '\xffb46' = "i"
logogramToText '\xffb47' = "iki"
logogramToText '\xffb48' = "ipi"
logogramToText '\xffb49' = "itomi"
logogramToText '\xffb4a' = "itomi"
logogramToText '\xffb4b' = "itomi"
logogramToText '\xffb4c' = "itomi"
logogramToText '\xffb4d' = "jaku"
logogramToText '\xffb4e' = "je"
logogramToText '\xffb4f' = "jonke"
logogramToText '\xffb50' = "jonke"
logogramToText '\xffb51' = "kapa"
logogramToText '\xffb52' = "kapa"
logogramToText '\xffb53' = "kapa"
logogramToText '\xffb54' = "kapa"
logogramToText '\xffb55' = "ki"
logogramToText '\xffb56' = "ki"
logogramToText '\xffb57' = "kisa"
logogramToText '\xffb58' = "konwe"
logogramToText '\xffb59' = "kosan"
logogramToText '\xffb5a' = "kosan"
logogramToText '\xffb5b' = "kosan"
logogramToText '\xffb5c' = "kosan"
logogramToText '\xffb5d' = "kulu"
logogramToText '\xffb5e' = "lokon"
logogramToText '\xffb5f' = "molusa"
logogramToText '\xffb60' = "nja"
logogramToText '\xffb61' = "okepuma"
logogramToText '\xffb62' = "omen"
logogramToText '\xffb63' = "oni"
logogramToText '\xffb64' = "oni"
logogramToText '\xffb65' = "owe"
logogramToText '\xffb66' = "pa"
logogramToText '\xffb67' = "pasila"
logogramToText '\xffb68' = "pasila"
logogramToText '\xffb69' = "peta"
logogramToText '\xffb6a' = "pika"
logogramToText '\xffb6b' = "pipo"
logogramToText '\xffb6c' = "sikomo"
logogramToText '\xffb6d' = "sikomo"
logogramToText '\xffb6e' = "sikomo"
logogramToText '\xffb6f' = "sikomo"
logogramToText '\xffb70' = "tokana"
logogramToText '\xffb71' = "wawajete"
logogramToText '\xffb72' = "we"
logogramToText '\xffb73' = "ako"
logogramToText '\xffb74' = "jume"
logogramToText '\xffb75' = "konsi"
logogramToText '\xffb76' = "konsi"
logogramToText '\xffb77' = "kutopoma"
logogramToText '\xffb78' = "lijokuku"
logogramToText '\xffb79' = "lijokuku"
logogramToText '\xffb7a' = "lo"
logogramToText '\xffb7b' = "lo"
logogramToText '\xffb7c' = "me"
logogramToText '\xffb7d' = "nele"
logogramToText '\xffb7e' = "nu"
logogramToText '\xffb7f' = "nuwa"
logogramToText '\xffb80' = "poni"
logogramToText '\xffb81' = "sipi"
logogramToText '\xffb82' = "wi"
logogramToText '\xffb83' = "wi"
logogramToText '\xffb84' = "oki"
logogramToText '\xffb85' = "to"
logogramToText '\xffb86' = "sowoli"
logogramToText '\xffb87' = "sowoli"
logogramToText '\xffb88' = "sowoli"
logogramToText '\xffb89' = "sowoli"
logogramToText '\xffb8a' = "kulepiku"
logogramToText '\xffb8b' = "epikule"
logogramToText '\xffb8c' = "yutu"
logogramToText '\xffb8d' = "slape"
logogramToText '\xffb8e' = "jans"
logogramToText '\xffb8f' = "jew"
logogramToText '\xffb90' = "a suli"
logogramToText '\xffb91' = "asuto"
logogramToText '\xffb92' = "eko"
logogramToText '\xffb93' = "jan jan"
logogramToText '\xffb94' = "kapilu"
logogramToText '\xffb95' = "kili ma"
logogramToText '\xffb96' = "kulupu en"
logogramToText '\xffb97' = "kulupu kasi"
logogramToText '\xffb98' = "nili"
logogramToText '\xffb99' = "unikijetesantakalu"
logogramToText '\xffb9a' = "waso pimeja"
logogramToText '\xffb9b' = "weko"
logogramToText '\xffb9c' = "nasulin"
logogramToText '\xffb9d' = "juniko"
logogramToText '\xffb9e' = "sijesuwa"
logogramToText '\xffb9f' = "jusijesuwa"
logogramToText '\xffba0' = "kalijopilale"
logogramToText '\xffba1' = "kijosin"
logogramToText '\xffba2' = "kijosin"
logogramToText '\xffba3' = "kijosin"
logogramToText '\xffba4' = "kijosin"
logogramToText '\xffba5' = "linluwi"
logogramToText '\xffba6' = "linluwi"
logogramToText '\xffba7' = "linluwi"
logogramToText '\xffba8' = "linluwi"
logogramToText '\xffba9' = "mamasi"
logogramToText '\xffbaa' = "waken"
logogramToText '\xffbab' = "lansan"
logogramToText '\xffbac' = "wiki"
logogramToText '\xffbad' = "ten"
logogramToText '\xffbae' = "tona"
logogramToText '\xffbaf' = "poki tona"
logogramToText '\xffbb0' = "ijo meli"
logogramToText '\xffbb1' = "ijo mije"
logogramToText '\xffbb2' = "ijo namako"
logogramToText '\xffbb3' = "ijo olin"
logogramToText '\xffbb4' = "ijo sewi"
logogramToText '\xffbb5' = "ijo uta"
logogramToText '\xffbb6' = "ijo wile"
logogramToText '\xffbb7' = "jan olin"
logogramToText '\xffbb8' = "jan sewi"
logogramToText '\xffbb9' = "kala lete"
logogramToText '\xffbba' = "kala lete"
logogramToText '\xffbbb' = "kala lili"
logogramToText '\xffbbc' = "kala lili"
logogramToText '\xffbbd' = "laso kasi"
logogramToText '\xffbbe' = "laso mun"
logogramToText '\xffbbf' = "laso sewi"
logogramToText '\xffbc0' = "laso sewi"
logogramToText '\xffbc1' = "laso telo"
logogramToText '\xffbc2' = "lipu nimi"
logogramToText '\xffbc3' = "lipu sewi"
logogramToText '\xffbc4' = "lipu tenpo"
logogramToText '\xffbc5' = "loje jelo"
logogramToText '\xffbc6' = "loje walo"
logogramToText '\xffbc7' = "luka luka"
logogramToText '\xffbc8' = "luka luka luka tu"
logogramToText '\xffbc9' = "luka luka luka tu tu"
logogramToText '\xffbca' = "luka luka luka tu wan"
logogramToText '\xffbcb' = "luka luka luka wan"
logogramToText '\xffbcc' = "luka luka luka wan tu"
logogramToText '\xffbcd' = "luka tu"
logogramToText '\xffbce' = "luka wan"
logogramToText '\xffbcf' = "lupa meli"
logogramToText '\xffbd0' = "mama meli"
logogramToText '\xffbd1' = "mama mije"
logogramToText '\xffbd2' = "mama tonsi"
logogramToText '\xffbd3' = "meli ike"
logogramToText '\xffbd4' = "meli lili"
logogramToText '\xffbd5' = "meli pona"
logogramToText '\xffbd6' = "meli sama"
logogramToText '\xffbd7' = "meli tonsi"
logogramToText '\xffbd8' = "meli tonsi"
logogramToText '\xffbd9' = "meli unpa"
logogramToText '\xffbda' = "meli wawa"
logogramToText '\xffbdb' = "meli wawa"
logogramToText '\xffbdc' = "mije ike"
logogramToText '\xffbdd' = "mije lili"
logogramToText '\xffbde' = "mije pona"
logogramToText '\xffbdf' = "mije sama"
logogramToText '\xffbe0' = "mije tonsi"
logogramToText '\xffbe1' = "mije tonsi"
logogramToText '\xffbe2' = "mije unpa"
logogramToText '\xffbe3' = "mije wawa"
logogramToText '\xffbe4' = "nena meli"
logogramToText '\xffbe5' = "tonsi ike"
logogramToText '\xffbe6' = "tonsi lili"
logogramToText '\xffbe7' = "tonsi pona"
logogramToText '\xffbe8' = "tonsi sama"
logogramToText '\xffbe9' = "tonsi sin"
logogramToText '\xffbea' = "tonsi unpa"
logogramToText '\xffbeb' = "tonsi wawa"
logogramToText '\xffbec' = "walo pimeja"
logogramToText '\xffbed' = "wawa tenpo"
logogramToText '\xffbee' = "ẞ"
logogramToText '\xffbef' = "ß"
logogramToText '\xffbf0' = "ma pona pi toki pona"
logogramToText '\xffbf1' = "ma pi nasin sitelen"
logogramToText '\xffbf2' = "linku"
logogramToText '\xffbf3' = "lipu wesi pi toki pona"
logogramToText '\xffbf4' = "linja sike"
logogramToText '\xffbf5' = "linja suwi"
logogramToText '\xffbf6' = "nasin nanpa"
logogramToText '\xffbf7' = "nasin nanpa"
logogramToText '\xffbf8' = "nasin sitelen"
logogramToText '\xffbf9' = "sitelen seli kiwen"
logogramToText '\xffbfa' = "nimi sin"
logogramToText '\xffbfb' = "nimi sin"
logogramToText '\xffbfc' = "amonka"
logogramToText '\xffbfd' = "manka"
logogramToText '\xffbfe' = "nutan"
logogramToText '\xffbff' = "ijo ni li seme"
logogramToText c = [c]