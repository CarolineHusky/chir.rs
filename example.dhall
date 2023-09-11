let SqliteConfig =
      { Type =
          { filename : Text
          , walEnabled : Optional Bool
          , fkEnabled : Optional Bool
          , extraPragmas : Optional (List Text)
          }
      , default =
        { walEnabled = None Bool
        , fkEnabled = None Bool
        , extraPragmas = None (List Text)
        }
      }

let PostgresConfig =
      { Type =
          { connectionString : Text
          , poolStripes : Natural
          , poolIdleTimeout : Natural
          }
      , default = { poolStripes = 0, poolIdleTimeout = 300 }
      }

let DatabaseConfig =
      { Type =
          < DatabaseSQLite : SqliteConfig.Type
          | DatabasePostgres : PostgresConfig.Type
          >
      }

let LogLevel =
      { Type =
          < LogLevelDebug
          | LogLevelInfo
          | LogLevelWarn
          | LogLevelError
          | LogLevelOther : Text
          >
      }

let Config =
      { Type =
          { listenPort : Natural
          , database : DatabaseConfig.Type
          , databasePoolSize : Natural
          , staticDir : Text
          , logLevel : LogLevel.Type
          }
      , default =
        { databasePoolSize = 10
        , staticDir = "./static"
        , logLevel = LogLevel.Type.LogLevelInfo
        }
      }

in  Config::{
    , listenPort = 62936
    , database =
        DatabaseConfig.Type.DatabaseSQLite
          SqliteConfig::{ filename = "database.db" }
    }
