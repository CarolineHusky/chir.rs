module Crypto.Auth.Token where

import Config (ConfigFile, rpId')
import Control.Lens (Lens', (.~), (?~), (^.))
import Crypto.JWT (Audience (Audience), ClaimsSet, HasClaimsSet (..), NumericDate (NumericDate), emptyClaimsSet, stringOrUri)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withObject, (.:))
import Data.Aeson.KeyMap qualified as M
import Data.Aeson.Types (Value (Object))
import Data.Time (UTCTime, addUTCTime, getCurrentTime)

data Claims = Claims
  { jwtClaims :: ClaimsSet
  , authTime :: Maybe NumericDate
  , nonce :: Maybe Text
  , acr :: Maybe Text
  , amr :: Maybe Text
  , azr :: Maybe Text
  }

claimAuthTime :: Lens' Claims (Maybe NumericDate)
claimAuthTime f h@Claims {authTime = a} = fmap (\a' -> h {authTime = a'}) (f a)

claimNonce :: Lens' Claims (Maybe Text)
claimNonce f h@Claims {nonce = a} = fmap (\a' -> h {nonce = a'}) (f a)

claimAcr :: Lens' Claims (Maybe Text)
claimAcr f h@Claims {acr = a} = fmap (\a' -> h {acr = a'}) (f a)

claimAmr :: Lens' Claims (Maybe Text)
claimAmr f h@Claims {amr = a} = fmap (\a' -> h {amr = a'}) (f a)

claimAzr :: Lens' Claims (Maybe Text)
claimAzr f h@Claims {azr = a} = fmap (\a' -> h {azr = a'}) (f a)

instance HasClaimsSet Claims where
  claimsSet :: Lens' Claims ClaimsSet
  claimsSet f s = fmap (\a' -> s {jwtClaims = a'}) (f (jwtClaims s))

instance FromJSON Claims where
  parseJSON = withObject "Claims" $ \o ->
    Claims
      <$> parseJSON (Object o)
      <*> o
      .: "auth_time"
      <*> o
      .: "nonce"
      <*> o
      .: "acr"
      <*> o
      .: "amr"
      <*> o
      .: "azr"

instance ToJSON Claims where
  toJSON s =
    ins "auth_time" (authTime s) $
      ins "nonce" (nonce s) $
        ins "acr" (acr s) $
          ins "amr" (amr s) $
            ins "azr" (azr s) $
              toJSON (jwtClaims s)
    where
      ins k v (Object o) = Object $ M.insert k (toJSON v) o
      ins _ _ a = a

emptyClaims :: Claims
emptyClaims = Claims emptyClaimsSet Nothing Nothing Nothing Nothing Nothing

stringConvert :: (ToString a, IsString b) => a -> b
stringConvert = fromString . toString

mkClaims :: ConfigFile -> Text -> Text -> Text -> UTCTime -> Maybe Text -> Text -> IO Claims
mkClaims config subject audience jti authTime nonce authParty = do
  now <- getCurrentTime
  let expires = addUTCTime 300 now
  pure $
    emptyClaims
      & claimIss ?~ stringConvert ("https://" <> config ^. rpId' <> "/")
      & claimSub ?~ stringConvert subject
      & claimAud ?~ Audience [stringConvert audience]
      & claimExp ?~ NumericDate expires
      & claimNbf ?~ NumericDate now
      & claimIat ?~ NumericDate now
      & claimJti ?~ jti
      & claimAuthTime ?~ NumericDate authTime
      & claimNonce .~ nonce
      & claimAcr ?~ "phrh"
      & claimAmr ?~ "hwk"
      & claimAzr .~ if audience == authParty then Nothing else Just authParty
