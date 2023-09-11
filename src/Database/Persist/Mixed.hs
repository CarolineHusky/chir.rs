module Database.Persist.Mixed where

import Config (ConfigFile (database, databasePoolSize), DatabaseConfig (..), toPostgresConf, toSqliteConfInfo)
import Control.Monad.Logger (MonadLoggerIO)
import Data.Pool (Pool)
import Database.Persist.Postgresql (createPostgresqlPoolWithConf, defaultPostgresConfHooks)
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Sqlite (createSqlitePoolFromInfo)
import Yesod (MonadUnliftIO)

create :: (MonadLoggerIO m, MonadUnliftIO m) => ConfigFile -> m (Pool SqlBackend)
create cfgFull = res
  where
    res = case database cfgFull of
      DatabaseSQLite cfg -> createSqlitePoolFromInfo (toSqliteConfInfo cfg) poolSize
      DatabasePostgres cfg -> createPostgresqlPoolWithConf (toPostgresConf cfg poolSize) defaultPostgresConfHooks
    poolSize = fromIntegral $ databasePoolSize cfgFull
