module Handler.FinishRegistration (postFinishRegistrationR) where

import Config (rpId')
import Control.Lens ((^.))
import Control.Monad.Logger (logError)
import Crypto.FidoMetadataManager (getMatadataBlobRegistry)
import Crypto.Hash (hash)
import Crypto.WebAuthn qualified as WA
import Data.Aeson (encode)
import Data.ByteString.Base64 qualified as B64
import Data.Hourglass (timeGetDateTimeOfDay)
import Data.Time (getCurrentTime)
import Data.Validation (Validation (Failure, Success))
import Database.Persist.Sql (PersistEntity (Key, keyFromValues), PersistUniqueWrite (insertUnique_), PersistValue (PersistText), deleteWhereCount, (==.), (>.))
import Foundation (App, appConfig, appHttpManager)
import Handler.StartRegistration (mkCredentialOptionsRegistration)
import Model (EntityField (WebauthnChallengeExpiresAt, WebauthnChallengeJti), LocalAccount (LocalAccount), LocalAccountCredentials (LocalAccountCredentials))
import Utils ((<<<$>>>))
import Yesod (HandlerFor, Value, YesodPersist (runDB), getYesod, invalidArgs, lookupHeader, permissionDenied, requireCheckJsonBody, returnJson)
import Yesod.Core (lookupGetParam)

metadataKey :: Text -> Key LocalAccount
metadataKey v = case keyFromValues [PersistText v] of
  Left e -> error ("Code error" <> e)
  Right v' -> v'

postFinishRegistrationR :: HandlerFor App Value
postFinishRegistrationR = do
  lookupGetParam "username" >>= \case
    Nothing -> invalidArgs ["username"]
    Just username -> do
      credential <- requireCheckJsonBody
      case WA.wjDecodeCredentialRegistration credential of
        Left e -> do
          print $ "Invalid body: " <> e
          invalidArgs ["body"]
        Right cred -> do
          let challenge = WA.ccdChallenge $ WA.arrClientData $ WA.cResponse cred
          now <- liftIO getCurrentTime
          valid_challenge <- runDB $ deleteWhereCount [WebauthnChallengeJti ==. decodeUtf8 (B64.encode $ WA.unChallenge challenge), WebauthnChallengeExpiresAt >. now]
          case valid_challenge of
            1 -> do
              let options = mkCredentialOptionsRegistration username challenge
              site <- getYesod
              let rpId = site ^. appConfig . rpId'
              let rpIdHash = WA.RpIdHash $ hash (encodeUtf8 rpId :: ByteString)
              registry <-
                runDB $
                  getMatadataBlobRegistry (site ^. appHttpManager) >>= \case
                    Just m -> return m
                    Nothing -> error "Can’t fetch metadata blob"
              origin <- fromMaybe rpId <$> (decodeUtf8 <<<$>>> lookupHeader "Host")
              case WA.verifyRegistrationResponse (WA.Origin ("https://" <> origin)) rpIdHash registry (timeGetDateTimeOfDay now) options cred of
                Failure errs -> do
                  $(logError) $ "Failed to verify registration: " <> show errs
                  permissionDenied "Failed to verify registration"
                Success result -> case WA.rrAttestationStatement result of
                  WA.SomeAttestationStatement {WA.asModel = WA.VerifiedAuthenticator _ _} -> do
                    -- We have a valid authenticator
                    result' <-
                      runDB $
                        insertUnique_ (LocalAccount username False) >>= \case
                          Nothing -> return $ Left ("Already exists" :: Text)
                          Just _ ->
                            let entry = WA.rrEntry result
                             in insertUnique_
                                  ( LocalAccountCredentials
                                      (WA.unCredentialId $ WA.ceCredentialId entry)
                                      (metadataKey username)
                                      (WA.unPublicKeyBytes $ WA.cePublicKeyBytes entry)
                                      (fromIntegral $ WA.unSignatureCounter $ WA.ceSignCounter entry)
                                      (toStrict $ encode $ WA.ceTransports entry)
                                  )
                                  >>= \case
                                    Nothing -> return $ Left "Failed to insert key"
                                    Just _ -> return $ Right ()
                    case result' of
                      Left e -> permissionDenied e
                      Right _ -> returnJson ()
                  _ -> permissionDenied "Failed to verify attestation"
            _ -> error "Failed to post finish registration"
