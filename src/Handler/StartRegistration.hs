module Handler.StartRegistration (getStartRegistrationR, mkCredentialOptionsRegistration) where

import Crypto.WebAuthn qualified as WA
import Data.Aeson (Value)
import Database.Persist (PersistEntity (Key, keyFromValues), PersistValue (PersistText))
import Database.Persist qualified as P
import Foundation (App)
import Handler.WebauthnChallenge (generateChallenge)
import Model (LocalAccount)
import Yesod (HandlerFor, YesodPersist (runDB), invalidArgs, lookupGetParam, returnJson)

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
getStartRegistrationR =
  lookupGetParam "username" >>= \case
    Just username ->
      let key :: Key LocalAccount = case keyFromValues [PersistText username] of
            Left e -> error $ show e
            Right k -> k
       in runDB (P.get key) >>= \case
            Just _ -> invalidArgs ["username"]
            Nothing -> do
              challenge <- generateChallenge
              returnJson $ WA.wjEncodeCredentialOptionsRegistration $ mkCredentialOptionsRegistration username challenge
    Nothing -> invalidArgs ["username"]
