module Config.Images where

import Control.Lens (makeLenses)
import Text.Internationalisation (Message (MsgImgSammyTheTanukiBabyLottePFP))

data Image = Image
  { _name :: Text
  , _mipmaps :: [(Int, Int)]
  , _maxWidth :: Int
  , _altText :: Message
  }

makeLenses ''Image

img_2023_10_26_sammythetanuki_babylottepfp :: Image
img_2023_10_26_sammythetanuki_babylottepfp =
  Image
    { _name = "2023-10-26-sammythetanuki-babylottepfp"
    , _mipmaps = [(576, 128), (256, 256)]
    , _maxWidth = 256
    , _altText = MsgImgSammyTheTanukiBabyLottePFP
    }
