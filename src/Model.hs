module Model (migrateAll) where

import Database.Persist.Quasi
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
