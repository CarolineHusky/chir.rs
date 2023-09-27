module Handler.Webfinger (getWebfingerR) where

import Data.Aeson (Value, object, (.=))
import Database.Persist (Entity (entityVal), (==.))
import Foundation (App)
import Model (EntityField (WebFingerAccountWebfingerUsername), WebFingerAccount (webFingerAccountIndieUsername, webFingerAccountOidcIssuer, webFingerAccountWebfingerUsername))
import Yesod (HandlerFor, PersistQueryRead (selectFirst), YesodPersist (runDB), lookupGetParam, notFound)
import Yesod.Core (invalidArgs)

mkWebfinger :: WebFingerAccount -> Value
mkWebfinger account =
  let
    oidcIssuer = case webFingerAccountOidcIssuer account of
      Just iss ->
        [ object
            [ "rel" .= ("http://openid.net/specs/connect/1.0/issuer" :: Text)
            , "href" .= iss
            ]
        ]
      Nothing -> []
    links =
      object
        [ "rel" .= ("http://webfinger.net/rel/profile-page" :: Text)
        , "href" .= webFingerAccountIndieUsername account
        ]
        : oidcIssuer
   in
    object
      [ "subject" .= webFingerAccountWebfingerUsername account
      , "links" .= links
      ]

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
          Just value -> return $ mkWebfinger $ entityVal value
