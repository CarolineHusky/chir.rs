module Handler.FinishRegistration (postFinishRegistrationR) where

import Config (rpId')
import Control.Alternative ((?), (?!))
import Control.Lens ((^.))
import Control.Monad.Logger (logError)
import Crypto.FidoMetadataManager (getMatadataBlobRegistry)
import Crypto.Hash (hash)
import Crypto.WebAuthn qualified as WA
import Data.Aeson (encode)
import Data.ByteString.Base64 qualified as B64
import Data.Hourglass (timeGetDateTimeOfDay)
import Data.Time (getCurrentTime)
import Database.Persist.Sql (PersistEntity (Key, keyFromValues), PersistUniqueWrite (insertUnique_), PersistValue (PersistText), deleteWhereCount, (==.), (>.))
import Foundation (App, appConfig, appHttpManager, requireJSONBody)
import Handler.StartRegistration (mkCredentialOptionsRegistration)
import Model (EntityField (WebauthnChallengeExpiresAt, WebauthnChallengeJti), LocalAccount (LocalAccount), LocalAccountCredentials (LocalAccountCredentials), WebFingerAccount (WebFingerAccount))
import Network.URL (URL (URL), importURL)
import Network.URL qualified as URL
import Network.URL.Normalize (normalizeURL)
import Utils ((<<<$>>>))
import Yesod (HandlerFor, YesodPersist (runDB), getYesod, invalidArgs, lookupHeader, permissionDenied)
import Yesod.Core (lookupGetParam)

metadataKey :: Text -> Key LocalAccount
metadataKey v = case keyFromValues [PersistText v] of
  Left e -> error ("Code error" <> e)
  Right v' -> v'

postFinishRegistrationR :: HandlerFor App ()
postFinishRegistrationR = do
  username' <- lookupGetParam "username" ?! invalidArgs ["username"]
  username <- pure (normalizeURL username') ?! invalidArgs ["username"]
  profile_host <- case importURL $ toString username of
    Just
      ( URL
          { URL.url_type =
            URL.Absolute
              ( URL.Host
                  { URL.host = host
                  }
                )
          }
        ) -> pure $ toText host
    _ -> invalidArgs ["username"]
  cred <-
    -- try to decode the registration message
    ( WA.wjDecodeCredentialRegistration
        <$> requireJSONBody
      ) -- or return error
      ? ( \e -> do
            $(logError) $ "Invalid body: " <> e
            invalidArgs ["body"]
        )
  let challenge = WA.ccdChallenge $ WA.arrClientData $ WA.cResponse cred
  -- check if challenge is valid
  now <- liftIO getCurrentTime
  valid_challenge <- runDB $ deleteWhereCount [WebauthnChallengeJti ==. decodeUtf8 (B64.encode $ WA.unChallenge challenge), WebauthnChallengeExpiresAt >. now]
  if valid_challenge == 1 then pass else permissionDenied "Failed to verify registration"
  -- gather necessary info
  let options = mkCredentialOptionsRegistration username challenge
  site <- getYesod
  let rpId = site ^. appConfig . rpId'
  let rpIdHash = WA.RpIdHash $ hash (encodeUtf8 rpId :: ByteString)
  origin <- fromMaybe rpId <$> (decodeUtf8 <<<$>>> lookupHeader "Host")
  -- open the FIDO metadata registry
  registry <-
    runDB $
      getMatadataBlobRegistry (site ^. appHttpManager)
        ?! error "Can’t fetch metadata blob"
  -- try to verify
  result@WA.RegistrationResult
    { WA.rrEntry =
      WA.CredentialEntry
        { WA.ceCredentialId = WA.CredentialId {WA.unCredentialId = credentialId}
        , WA.cePublicKeyBytes = WA.PublicKeyBytes {WA.unPublicKeyBytes = publicKeyBytes}
        , WA.ceSignCounter = WA.SignatureCounter {WA.unSignatureCounter = signatureCounter}
        , WA.ceTransports = transports
        }
    } <-
    pure (WA.verifyRegistrationResponse (WA.Origin ("https://" <> origin)) rpIdHash registry (timeGetDateTimeOfDay now) options cred)
      ? \errs -> do
        $(logError) $ "Failed to verify registration: " <> show errs
        permissionDenied "Failed to verify registration"
  case WA.rrAttestationStatement result of
    WA.SomeAttestationStatement {WA.asModel = WA.VerifiedAuthenticator _ _} -> do
      -- We have a valid authenticator
      runDB $ do
        insertUnique_ (LocalAccount username False) ?! permissionDenied "Registration error"
        insertUnique_
          ( LocalAccountCredentials
              credentialId
              (metadataKey username)
              publicKeyBytes
              (fromIntegral signatureCounter)
              $ toStrict
              $ encode transports
          )
          ?! permissionDenied "Registration error"
        insertUnique_ (WebFingerAccount ("acct:" <> profile_host <> "@" <> profile_host) username $ Just username) ?! permissionDenied "Registration error"
        pass
    _ -> permissionDenied "Failed to verify attestation"
