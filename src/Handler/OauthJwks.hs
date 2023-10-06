module Handler.OauthJwks where

import Control.Lens ((^?))
import Crypto.JOSE (AsPublicKey (asPublicKey), Crv (P_256), KeyMaterialGenParam (RSAGenParam))
import Crypto.JOSE.JWA.JWK (KeyMaterialGenParam (ECGenParam))
import Crypto.KeyStore (getKeyWithRekey)
import Data.Aeson (KeyValue ((.=)), Value, object)
import Foundation (App, returnJSON)
import Yesod (HandlerFor, TypedContent, array)
import Yesod.Persist (runDB)

getOauthJwksR :: HandlerFor App TypedContent
getOauthJwksR = do
  es256 <- runDB $ getKeyWithRekey "oauth_es256" (ECGenParam P_256) 90
  rs256 <- runDB $ getKeyWithRekey "oauth_rs256" (RSAGenParam 256) 90

  returnJSON $
    object
      [ "keys"
          .= array
            [ es256 ^? asPublicKey
            , rs256 ^? asPublicKey
            ]
      ]
