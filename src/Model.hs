module Model where

import Data.Time (UTCTime)
import Database.Persist.Quasi (lowerCaseSettings)
import Database.Persist.TH (
  mkMigrate,
  mkPersist,
  persistFileWith,
  share,
  sqlSettings,
 )

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models.persistentmodels")
