module Config.Images where

import Config.StaticFiles
import Control.Lens (makeLenses)
import Foundation (App, Route (StaticR))
import Text.Internationalisation (Message (MsgImgVintagecoyotePrideicon))

data Image = Image
  { _jxl :: Route App
  , _avif :: Route App
  , _heif :: Route App
  , _webp :: Route App
  , _jpg :: Route App
  , _png :: Route App
  , _altText :: Message
  }

makeLenses ''Image

img_2023_06_02_vintagecoyote_prideicon :: Image
img_2023_06_02_vintagecoyote_prideicon =
  Image
    { _jxl = StaticR img_2023_06_02_vintagecoyote_prideicon_jxl
    , _avif = StaticR img_2023_06_02_vintagecoyote_prideicon_avif
    , _heif = StaticR img_2023_06_02_vintagecoyote_prideicon_heif
    , _webp = StaticR img_2023_06_02_vintagecoyote_prideicon_webp
    , _jpg = StaticR img_2023_06_02_vintagecoyote_prideicon_jpg
    , _png = StaticR img_2023_06_02_vintagecoyote_prideicon_png
    , _altText = MsgImgVintagecoyotePrideicon
    }
