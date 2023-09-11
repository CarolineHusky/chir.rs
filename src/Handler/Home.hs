module Handler.Home (getHomeR) where

import Foundation (App)
import Yesod (HandlerFor, Html, Yesod (defaultLayout), whamlet)

getHomeR :: HandlerFor App Html
getHomeR = defaultLayout [whamlet|Hewo!|]
