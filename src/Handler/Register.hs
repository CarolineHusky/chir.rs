module Handler.Register (getRegisterR) where

import Config (widgetFile)
import Config.StaticFiles (register_js)
import Foundation (App, Route (StaticR))
import Yesod (HandlerFor, Html, Yesod (defaultLayout), addScript, setTitle)

getRegisterR :: HandlerFor App Html
getRegisterR = do
  defaultLayout $ do
    setTitle "Registration"
    addScript $ StaticR register_js
    $(widgetFile "register")
