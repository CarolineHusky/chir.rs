module Components.Image where

import Config.Images (Image, altText, maxWidth, mipmaps, name)
import Control.Lens ((^.))
import Foundation (App, Route (StaticR, StaticRoute))
import Text.Internationalisation (__)
import Yesod (whamlet)
import Yesod.Core (WidgetFor)

getRouteFor :: Text -> Text -> Int -> Route App
getRouteFor iname ext size =
  let
    fname = iname <> "-" <> show size <> "." <> ext
   in
    StaticR $ StaticRoute ["img", fname] []

srcsetEntry :: Text -> Text -> (Int, Int) -> WidgetFor App ()
srcsetEntry iname ext (_, size) =
  let
    route = getRouteFor iname ext size
   in
    [whamlet|@{route} #{size}w|]

srcsets :: Text -> Text -> [(Int, Int)] -> WidgetFor App ()
srcsets _ _ [] = [whamlet||]
srcsets iname ext [size] =
  let headWidget = srcsetEntry iname ext size
   in [whamlet|^{headWidget}|]
srcsets iname ext (size : rest) =
  let headWidget = srcsetEntry iname ext size
      tailWidget = srcsets iname ext rest
   in [whamlet|^{tailWidget},^{headWidget}|]

sizesEntry :: (Int, Int) -> WidgetFor App ()
sizesEntry (maxWidth', width)
  | maxWidth' == width = [whamlet|#{width}px|]
  | otherwise = [whamlet|(max-width: #{maxWidth'}px) #{width}px|]

sizes :: [(Int, Int)] -> WidgetFor App ()
sizes [] = [whamlet||]
sizes [entry] =
  let headWidget = sizesEntry entry
   in [whamlet|^{headWidget}|]
sizes (entry : rest) =
  let headWidget = sizesEntry entry
      tailWidget = sizes rest
   in [whamlet|^{tailWidget},^{headWidget}|]

sourceEntry :: Image -> Text -> WidgetFor App ()
sourceEntry img ext =
  let
    srcset = srcsets (img ^. name) ext (img ^. mipmaps)
    sizes' = sizes (img ^. mipmaps)
   in
    [whamlet|$newline never
      <source srcset="^{srcset}" sizes="^{sizes'}" type="image/#{ext}" loading="lazy" decoding="async">|]

finalEntry :: Image -> WidgetFor App ()
finalEntry img =
  let route = getRouteFor (img ^. name) "png" (img ^. maxWidth)
   in [whamlet|$newline never
      <img src=@{route} type="img/png" alt=^{__ (img ^. altText)} loading="lazy" decoding="async">|]

image :: Image -> WidgetFor App ()
image img =
  [whamlet|$newline never
  <picture>
    ^{sourceEntry img "jxl"}
    ^{sourceEntry img "avif"}
    ^{sourceEntry img "heif"}
    ^{sourceEntry img "webp"}
    ^{sourceEntry img "jpeg"}
    ^{sourceEntry img "png"}
    ^{finalEntry img}
|]
