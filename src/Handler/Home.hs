module Handler.Home (getHomeR) where

import Components.Image (image)
import Config (widgetFile)
import Config.Images (img_2023_10_26_sammythetanuki_babylottepfp)
import Foundation (App, Route (HomeR))
import Text.Internationalisation (Message (..), getTranslation, __)
import Yesod (HandlerFor, Html, Yesod (defaultLayout), setTitle)

getHomeR :: HandlerFor App Html
getHomeR = do
  defaultLayout $ do
    getTranslation MsgPageTitle >>= setTitle
    $(widgetFile "homepage")
