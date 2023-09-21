module Handler.WebauthnChallenge (getWebauthnChallengeR) where

import Control.Lens ((?~))
import Crypto.JOSE (JWK, KeyMaterialGenParam (OctGenParam), bestJWSAlg, encodeCompact, newJWSHeader, runJOSE)
import Crypto.JWT (ClaimsSet, HasClaimsSet (claimExp, claimJti), JWTError, NumericDate (NumericDate), SignedJWT, emptyClaimsSet, signClaims)
import Crypto.KeyStore (getKeyWithRekey)
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Clock (NominalDiffTime)
import Foundation (App)
import System.Random (randomIO)
import Yesod (HandlerFor, YesodPersist (runDB))

mkClaims :: IO ClaimsSet
mkClaims = do
  t <- getCurrentTime
  let expiry = addUTCTime (60 :: NominalDiffTime) t
  jti :: Word64 <- randomIO
  pure $
    emptyClaimsSet
      & claimExp ?~ NumericDate expiry
      & claimJti ?~ show jti

mkJWT :: JWK -> IO (Either JWTError SignedJWT)
mkJWT jwk = runJOSE $ do
  alg <- bestJWSAlg jwk
  claims <- liftIO mkClaims
  signClaims jwk (newJWSHeader ((), alg)) claims

getWebauthnChallengeR :: HandlerFor App Text
getWebauthnChallengeR = do
  jwk <- runDB $ getKeyWithRekey "webauthnChallenge" (OctGenParam 32) 30
  jwt' <- liftIO $ mkJWT jwk
  case jwt' of
    Right jwt -> return $ decodeUtf8 $ encodeCompact jwt
    Left e -> error $ show e
