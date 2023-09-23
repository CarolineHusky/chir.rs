module Crypto.FidoMetadataManager (deleteMetadataBlobInfo, getMatadataBlobRegistry) where

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Control.Monad.Logger (MonadLogger, logError, logWarn)
import Control.Monad.Trans.Resource (MonadUnliftIO, runResourceT)
import Crypto.WebAuthn (MetadataServiceRegistry)
import Crypto.WebAuthn qualified as WA
import Data.ByteString.Lazy qualified as L
import Data.Hourglass (timeGetDateTimeOfDay)
import Data.Hourglass.Types (DateTime)
import Data.Queue (runTaskIn)
import Data.These (These (..))
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist (
  PersistEntity (keyFromValues),
  PersistValue (PersistText),
 )
import Database.Persist qualified as P
import Database.Persist.Postgresql (PersistEntity (Key), SqlPersistT)
import Foundation (QueueCommands (RefetchFidoMetadata))
import Model (KeyValueBlob (KeyValueBlob, keyValueBlobValue))
import Network.HTTP.Conduit (
  HttpException,
  Manager,
  Response (responseBody),
  httpLbs,
  parseRequest_,
 )
import Utils (catchM, (<<<$>>>))

data MetadataBlobInfo = MetadataBlobInfo
  { blob :: ByteString
  , fetchedAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance Serialise MetadataBlobInfo

metadataKey :: Key KeyValueBlob
metadataKey = case keyFromValues [PersistText "fidoMetadata"] of
  Left e -> error ("Code error" <> e)
  Right v -> v

tryGetMetadataBlobInfo :: (MonadUnliftIO m) => SqlPersistT m (Maybe MetadataBlobInfo)
tryGetMetadataBlobInfo = do
  P.get metadataKey >>= \case
    Nothing -> return Nothing
    Just blob' -> case deserialiseOrFail $ toLazy $ keyValueBlobValue blob' of
      Right v -> return $ Just v
      Left _ -> return Nothing

deleteMetadataBlobInfo :: (MonadUnliftIO m) => SqlPersistT m ()
deleteMetadataBlobInfo = P.delete metadataKey

fetchMetadataBlobInfo :: (MonadLogger m, MonadUnliftIO m) => Manager -> m (Maybe MetadataBlobInfo)
fetchMetadataBlobInfo manager = do
  res <- runResourceT $ do
    let req = parseRequest_ "https://mds.fidoalliance.org/"
    res' :: Either HttpException (Response L.ByteString) <- catchM (httpLbs req manager)
    now <- liftIO getCurrentTime
    return ((\blob -> MetadataBlobInfo {blob = blob, fetchedAt = now}) . toStrict . responseBody <$> res')
  case res of
    Left e -> do
      $(logError) $ "Failed to fetch metadata blob info from mds.fidoalliance.org: " <> show e
      return Nothing
    Right v -> return $ Just v

insertMetadataBlobInfo :: (MonadUnliftIO m) => MetadataBlobInfo -> SqlPersistT m ()
insertMetadataBlobInfo info = do
  deleteMetadataBlobInfo
  _ <- P.insert (KeyValueBlob "fidoMetadata" $ toStrict $ serialise info)
  runTaskIn RefetchFidoMetadata 2419200
  pass

getMetadataBlobInfo :: (MonadLogger m, MonadUnliftIO m) => Manager -> SqlPersistT m (Maybe (ByteString, DateTime))
getMetadataBlobInfo manager =
  tryGetMetadataBlobInfo >>= \case
    Just metadata -> return $ Just (blob metadata, timeGetDateTimeOfDay $ fetchedAt metadata)
    Nothing ->
      fetchMetadataBlobInfo manager >>= \case
        Nothing -> return Nothing
        Just metadata -> do
          insertMetadataBlobInfo metadata
          return $ Just (blob metadata, timeGetDateTimeOfDay $ fetchedAt metadata)

getMatadataBlobRegistry :: (MonadLogger m, MonadUnliftIO m) => Manager -> SqlPersistT m (Maybe MetadataServiceRegistry)
getMatadataBlobRegistry manager =
  (uncurry WA.metadataBlobToRegistry <<<$>>> getMetadataBlobInfo manager) >>= \case
    Nothing -> return Nothing
    Just (Left e) -> do
      $(logError) $ "Failed to decode metadata blob: " <> show e
      return Nothing
    Just (Right (This a)) -> do
      $(logError) $ "Failed to decode metadata blob: " <> show a
      return Nothing
    Just (Right (That a)) -> return $ Just a
    Just (Right (These e a)) -> do
      $(logWarn) $ "Failed to decode part of metadata blob: " <> show e
      return $ Just a
