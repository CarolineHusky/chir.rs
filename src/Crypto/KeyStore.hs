module Crypto.KeyStore (getKeyWithRekey, performRekey) where

import Control.Monad.Trans.Resource (MonadUnliftIO)
import Crypto.JOSE (KeyMaterial, KeyMaterialGenParam, genKeyMaterial)
import Crypto.KeyStore.Types (KeyMaterialGenParam', genParamFromJose, genParamToJose)
import Data.Aeson (decode, encode)
import Data.Queue (runTaskIn)
import Database.Persist qualified as P
import Database.Persist.Postgresql (PersistEntity (Key, keyFromValues), PersistValue (PersistText), SqlPersistT)
import Foundation (QueueCommands (Rekey))
import Model (Keys (..))

nameToKey :: Text -> Key Keys
nameToKey name = case keyFromValues [PersistText name] of
  Left e -> error ("Code error" <> e)
  Right v -> v

tryGetKey :: (MonadUnliftIO m) => Text -> SqlPersistT m (Maybe KeyMaterial)
tryGetKey name = do
  key <- P.get $ nameToKey name
  case key of
    Nothing -> return Nothing
    Just v -> return $ decode $ toLazy $ keysJwk v

deleteKey :: (MonadUnliftIO m) => Text -> SqlPersistT m ()
deleteKey name = P.delete $ nameToKey name

insertKey :: (MonadUnliftIO m) => Text -> KeyMaterial -> SqlPersistT m ()
insertKey name keyMaterial = do
  deleteKey name -- delete the existing key, if any
  _ <- P.insert $ Keys name $ toStrict $ encode keyMaterial
  pass

genKeyWithRekey :: (MonadUnliftIO m) => Text -> KeyMaterialGenParam -> Int -> SqlPersistT m KeyMaterial
genKeyWithRekey name parms days = do
  material <- liftIO $ genKeyMaterial parms
  insertKey name material
  if days > 0
    then do
      runTaskIn (Rekey name (genParamFromJose parms) days) (fromIntegral (days * 86_400))
    else pass
  return material

getKeyWithRekey :: (MonadUnliftIO m) => Text -> KeyMaterialGenParam -> Int -> SqlPersistT m KeyMaterial
getKeyWithRekey name parms days = do
  dbKey <- tryGetKey name
  case dbKey of
    Just key -> return key
    Nothing -> genKeyWithRekey name parms days

performRekey :: (MonadUnliftIO m) => Text -> KeyMaterialGenParam' -> Int -> SqlPersistT m ()
performRekey name parms days = genKeyWithRekey name (genParamToJose parms) days >> pass
