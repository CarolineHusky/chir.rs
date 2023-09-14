{-# LANGUAGE CPP #-}

module Database.Persist.Mixed (createConn) where

import Config (
  ConfigFile (database, databasePoolSize),
  DatabaseConfig (..),
 )
#ifdef POSTGRESQL
import Config (toPostgresConf)
#endif
#ifdef SQLITE
import Config (toSqliteConfInfo)
#endif
import Control.Monad.Logger (MonadLoggerIO)
import Data.Pool (Pool)
#ifdef POSTGRESQL
import Database.Persist.Postgresql (createPostgresqlPoolWithConf, defaultPostgresConfHooks)
#endif
import Database.Persist.Sql (SqlBackend)
#ifdef SQLITE
import Database.Persist.Sqlite (createSqlitePoolFromInfo)
#endif
import Yesod (MonadUnliftIO)

createConn :: (MonadLoggerIO m, MonadUnliftIO m) => ConfigFile -> m (Pool SqlBackend)
#if defined(SQLITE) || defined (POSTGRESQL)
createConn cfgFull = res
  where
    res = case database cfgFull of
#ifdef SQLITE
      DatabaseSQLite cfg -> createSqlitePoolFromInfo (toSqliteConfInfo cfg) poolSize
#endif
#ifdef POSTGRESQL
      DatabasePostgres cfg -> createPostgresqlPoolWithConf (toPostgresConf cfg poolSize) defaultPostgresConfHooks
#endif
#if !(defined(SQLITE) && defined(POSTGRESQL))
      _ -> error "Unknown/unsupported database"
#endif
    poolSize = fromIntegral $ databasePoolSize cfgFull
#else
createConn _ = error "Database support is disabled"
#endif
