module Handler.StartRegistration (getStartRegistrationR, mkCredentialOptionsRegistration) where

import Config (signUpKey')
import Control.Lens ((^.))
import Crypto.WebAuthn qualified as WA
import Data.Aeson (Value)
import Database.Persist (PersistEntity (Key, keyFromValues), PersistValue (PersistText))
import Database.Persist qualified as P
import Foundation (App, appConfig)
import Handler.WebauthnChallenge (generateChallenge)
import Model (LocalAccount)
import Network.URL.Normalize (normalizeURL)
import Utils ((?!))
import Yesod (HandlerFor, YesodPersist (runDB), invalidArgs, lookupGetParam, permissionDenied, returnJson)
import Yesod.Core (getYesod)

mkCredentialOptionsRegistration :: Text -> WA.Challenge -> WA.CredentialOptions 'WA.Registration
mkCredentialOptionsRegistration username challenge =
  WA.CredentialOptionsRegistration
    { WA.corRp = WA.CredentialRpEntity {WA.creId = Nothing, WA.creName = "Raccoon Authenticator"}
    , WA.corUser =
        WA.CredentialUserEntity
          { WA.cueId = WA.UserHandle {WA.unUserHandle = encodeUtf8 username}
          , WA.cueDisplayName =
              WA.UserAccountDisplayName
                { WA.unUserAccountDisplayName = username
                }
          , WA.cueName =
              WA.UserAccountName
                { WA.unUserAccountName = username
                }
          }
    , WA.corChallenge = challenge
    , WA.corPubKeyCredParams =
        [ {-WA.CredentialParameters
            { WA.cpTyp = WA.CredentialTypePublicKey
            , WA.cpAlg = WA.CoseAlgorithmEdDSA
            },-}
          WA.CredentialParameters
            { WA.cpTyp = WA.CredentialTypePublicKey
            , WA.cpAlg = WA.CoseAlgorithmES512
            }
        , WA.CredentialParameters
            { WA.cpTyp = WA.CredentialTypePublicKey
            , WA.cpAlg = WA.CoseAlgorithmES384
            }
        , WA.CredentialParameters
            { WA.cpTyp = WA.CredentialTypePublicKey
            , WA.cpAlg = WA.CoseAlgorithmES256
            }
        ]
    , WA.corTimeout = Just $ WA.Timeout {WA.unTimeout = 60_000}
    , WA.corExcludeCredentials = []
    , WA.corAuthenticatorSelection =
        Just $
          WA.AuthenticatorSelectionCriteria
            { WA.ascAuthenticatorAttachment = Just WA.AuthenticatorAttachmentCrossPlatform
            , WA.ascResidentKey = WA.ResidentKeyRequirementRequired
            , WA.ascUserVerification = WA.UserVerificationRequirementRequired
            }
    , WA.corAttestation = WA.AttestationConveyancePreferenceDirect
    , WA.corExtensions = Nothing
    }

getStartRegistrationR :: HandlerFor App Value
getStartRegistrationR = do
  app <- getYesod
  username' <- lookupGetParam "username" ?! invalidArgs ["username"]
  username <- pure (normalizeURL username') ?! invalidArgs ["username"]
  signup_token <- lookupGetParam "signup_token" ?! invalidArgs ["signup_token"]
  if signup_token == (app ^. (appConfig . signUpKey')) then pass else permissionDenied "signup_token"
  let key :: Key LocalAccount = case keyFromValues [PersistText username] of
        Right v -> v
        Left e -> error $ show e
  runDB (P.get key) >>= \case
    Just _ -> invalidArgs ["username"]
    Nothing -> pass
  challenge <- generateChallenge
  returnJson $ WA.wjEncodeCredentialOptionsRegistration $ mkCredentialOptionsRegistration username challenge
