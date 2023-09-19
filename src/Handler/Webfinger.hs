module Handler.Webfinger (getWebfingerR) where

import Data.Aeson (Value)
import Foundation (App)
import Network.Webfinger qualified as WF
import Network.Webfinger.Link qualified as WFL
import Yesod (HandlerFor, lookupGetParam, notFound, returnJson)
import Yesod.Core (invalidArgs)

webfinger :: WF.Webfinger
webfinger =
  WF.Webfinger
    { WF._subject = "acct:lotte@lotte.chir.rs"
    , WF._aliases = Nothing
    , WF._properties = Nothing
    , WF._links =
        Just
          [ WFL.WebfingerLink
              { -- openid connect
                WFL._rel = "http://openid.net/specs/connect/1.0/issue"
              , WFL._type = Nothing
              , WFL._href = "https://lotte.chir.rs/"
              , WFL._titles = Nothing
              , WFL._properties = Nothing
              }
          ]
    }

getWebfingerR :: HandlerFor App Value
getWebfingerR = do
  maybeAcct <- lookupGetParam "resource"
  case maybeAcct of
    Nothing -> invalidArgs ["resource"]
    Just "acct:lotte@lotte.chir.rs" -> returnJson webfinger
    _ -> notFound