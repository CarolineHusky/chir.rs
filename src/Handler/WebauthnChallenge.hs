module Handler.WebauthnChallenge (cleanupWebauthnChallenge, generateChallenge) where

import Control.Concurrent (threadDelay)
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Crypto.WebAuthn (Challenge)
import Crypto.WebAuthn qualified as WA
import Data.ByteString.Base64 qualified as B64
import Data.Time (addUTCTime, getCurrentTime)
import Data.Time.Clock (NominalDiffTime)
import Database.Persist (PersistQueryWrite (deleteWhere), PersistStoreWrite (insert_), (<=.))
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (ConnectionPool)
import Foundation (App)
import Model (EntityField (WebauthnChallengeExpiresAt), WebauthnChallenge (WebauthnChallenge))
import Yesod (HandlerFor, YesodPersist (runDB))

generateChallenge :: HandlerFor App Challenge
generateChallenge = do
  challenge <- liftIO WA.generateChallenge
  now <- liftIO getCurrentTime
  let expiry = addUTCTime (300 :: NominalDiffTime) now
  runDB $ insert_ $ WebauthnChallenge (decodeUtf8 $ B64.encode $ WA.unChallenge challenge) expiry
  return challenge

cleanupWebauthnChallenge :: (MonadUnliftIO m) => ConnectionPool -> m Void
cleanupWebauthnChallenge pool = infinitely $ do
  now <- liftIO getCurrentTime
  runSqlPool (deleteWhere [WebauthnChallengeExpiresAt <=. now]) pool
  liftIO $ threadDelay 300_000_000
  pass
