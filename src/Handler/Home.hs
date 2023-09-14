module Handler.Home (getHomeR) where

import Config (widgetFile)
import Foundation (App)
import Yesod (HandlerFor, Html, Yesod (defaultLayout))

getHomeR :: HandlerFor App Html
getHomeR = do
  defaultLayout $ do
    $(widgetFile "homepage")
