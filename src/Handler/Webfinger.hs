module Handler.Webfinger (getWebfingerR) where

import Data.Aeson (Value)
import Database.Persist (Entity (entityVal), (==.))
import Foundation (App)
import Model (EntityField (WebFingerAccountWebfingerUsername), WebFingerAccount (webFingerAccountIndieUsername, webFingerAccountOidcIssuer, webFingerAccountWebfingerUsername))
import Network.Webfinger qualified as WF
import Network.Webfinger.Link qualified as WFL
import Yesod (HandlerFor, PersistQueryRead (selectFirst), YesodPersist (runDB), lookupGetParam, notFound, returnJson)
import Yesod.Core (invalidArgs)

mkWebfinger :: WebFingerAccount -> WF.Webfinger
mkWebfinger account =
  let
    oidcIssuer = case webFingerAccountOidcIssuer account of
      Just iss ->
        [ WFL.WebfingerLink
            { -- openid connect
              WFL._rel = "http://openid.net/specs/connect/1.0/issuer"
            , WFL._type = Nothing
            , WFL._href = iss
            , WFL._titles = Nothing
            , WFL._properties = Nothing
            }
        ]
      Nothing -> []
    links =
      WFL.WebfingerLink
        { WFL._rel = "http://webfinger.net/rel/profile-page"
        , WFL._type = Nothing
        , WFL._href = webFingerAccountIndieUsername account
        , WFL._titles = Nothing
        , WFL._properties = Nothing
        }
        : oidcIssuer
   in
    WF.Webfinger
      { WF._subject = webFingerAccountWebfingerUsername account
      , WF._aliases = Nothing
      , WF._properties = Nothing
      , WF._links = Just links
      }

getWebfingerR :: HandlerFor App Value
getWebfingerR = do
  maybeAcct <- lookupGetParam "resource"
  case maybeAcct of
    Nothing -> invalidArgs ["resource"]
    Just acct -> do
      runDB $ do
        user <- selectFirst [WebFingerAccountWebfingerUsername ==. acct] []
        case user of
          Nothing -> notFound
          Just value -> returnJson $ mkWebfinger $ entityVal value
