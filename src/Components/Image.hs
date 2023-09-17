module Components.Image where

import Config (widgetFile)
import Config.Images (Image, altText, avif, heif, jpg, jxl, png, webp)
import Control.Lens ((^.))
import Foundation (App)
import Yesod.Core (WidgetFor)

image :: Image -> WidgetFor App ()
image img =
  let jxlImg = img ^. jxl
      avifImg = img ^. avif
      heifImg = img ^. heif
      webpImg = img ^. webp
      jpgImg = img ^. jpg
      pngImg = img ^. png
      alt = img ^. altText
   in $(widgetFile "image")
