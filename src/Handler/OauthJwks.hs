module Handler.OauthJwks where

import Control.Lens ((^?))
import Crypto.JOSE (AsPublicKey (asPublicKey), Crv (P_256), KeyMaterialGenParam (RSAGenParam))
import Crypto.JOSE.JWA.JWK (KeyMaterialGenParam (ECGenParam))
import Crypto.KeyStore (getKeyWithRekey)
import Data.Aeson (KeyValue ((.=)), Value, object)
import Foundation (App)
import Yesod (HandlerFor, array)
import Yesod.Persist (runDB)

getOauthJwksR :: HandlerFor App Value
getOauthJwksR = do
  es256 <- runDB $ getKeyWithRekey "oauth_es256" (ECGenParam P_256) 90
  rs256 <- runDB $ getKeyWithRekey "oauth_rs256" (RSAGenParam 256) 90

  return $
    object
      [ "keys"
          .= array
            [ es256 ^? asPublicKey
            , rs256 ^? asPublicKey
            ]
      ]
