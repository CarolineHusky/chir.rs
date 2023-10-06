module Crypto.KeyStore (getKeyWithRekey, performRekey) where

import Control.Lens ((?~))
import Control.Monad.Trans.Resource (MonadUnliftIO)
import Crypto.JOSE (JWK, KeyMaterialGenParam, genJWK)
import Crypto.JOSE.JWK (jwkKid)
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

tryGetKey :: (MonadUnliftIO m) => Text -> SqlPersistT m (Maybe JWK)
tryGetKey name = do
  key <- P.get $ nameToKey name
  case key of
    Nothing -> return Nothing
    Just v -> return $ decode $ toLazy $ keysJwk v

deleteKey :: (MonadUnliftIO m) => Text -> SqlPersistT m ()
deleteKey name = P.delete $ nameToKey name

insertKey :: (MonadUnliftIO m) => Text -> JWK -> SqlPersistT m ()
insertKey name jwk = do
  deleteKey name -- delete the existing key, if any
  _ <- P.insert $ Keys name $ toStrict $ encode jwk
  pass

genKeyWithRekey :: (MonadUnliftIO m) => Text -> KeyMaterialGenParam -> Int -> SqlPersistT m JWK
genKeyWithRekey name parms days = do
  material' <- liftIO $ genJWK parms
  let material = material' & jwkKid ?~ name
  insertKey name material
  if days > 0
    then do
      runTaskIn (Rekey name parms days) (fromIntegral (days * 86_400))
    else pass
  return material

getKeyWithRekey :: (MonadUnliftIO m) => Text -> KeyMaterialGenParam -> Int -> SqlPersistT m JWK
getKeyWithRekey name parms days = do
  dbKey <- tryGetKey name
  case dbKey of
    Just key -> return key
    Nothing -> genKeyWithRekey name parms days

performRekey :: (MonadUnliftIO m) => Text -> KeyMaterialGenParam -> Int -> SqlPersistT m ()
performRekey name parms days = genKeyWithRekey name parms days >> pass
