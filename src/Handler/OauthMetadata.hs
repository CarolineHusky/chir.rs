module Handler.OauthMetadata where

import Config (rpId')
import Control.Lens ((^.))
import Data.Aeson (KeyValue ((.=)), Value)
import Foundation (App, appConfig)
import Yesod (HandlerFor, array, getYesod, object)

getOauthMetadataR :: HandlerFor App Value
getOauthMetadataR = do
  app <- getYesod
  let sysUrl = "https://" <> app ^. appConfig . rpId'
  return $
    object
      [ "issuer" .= sysUrl
      , "authorization_endpoint" .= (sysUrl <> "/auth/login")
      , "token_endpoint" .= (sysUrl <> "/auth/token")
      , "userinfo_endpoint" .= (sysUrl <> "/auth/userinfo")
      , "jwks_uri" .= (sysUrl <> "/auth/jwks")
      , "registration_endpoint" .= (sysUrl <> "/auth/client_registration")
      , "scopes_supported" .= array ["openid" :: Text, "profile"]
      , "response_types_supported" .= array ["code" :: Text, "id_token", "token id_token"]
      , "response_modes_supported" .= array ["query" :: Text]
      , "grant_types_supported" .= array ["authorization_code" :: Text, "implicit"]
      , "acr_values_supported" .= array ["phrh" :: Text]
      , "subject_types_supported" .= array ["public" :: Text]
      , "id_token_signing_alg_values_supported" .= array ["Ed25519" :: Text, "ES256", "RS256" {- Bleichenbacher Town -}]
      , "ui_locales_supported" .= array ["en" :: Text, "de", "fr", "nl", "tok", "tok-Latn", "tok-QSIP", "tok-QSEM", "jbo", "jbo-Latn", "jbo-QZLR"]
      , "introspection_endpoint" .= (sysUrl <> "/auth/introspection")
      , "introspection_endpoint_auth_methods_supported" .= ["token" :: Text]
      , "revocation_endpoint" .= (sysUrl <> "/auth/revocation")
      , "revocation_endpoint_auth_methods_supported" .= ["none" :: Text]
      , "code_challenge_methods_supported" .= ["S256" :: Text]
      , "authorization_response_iss_parameter_supported" .= True
      ]
