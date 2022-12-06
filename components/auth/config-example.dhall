let DatabaseConfig =
      { Type =
          { db_url : Text
          , max_pool_size : Natural
          , min_pool_idle : Optional Natural
          }
      , default = { max_pool_size = 10, min_pool_idle = None Natural }
      }

let BaseConfig =
      { Type = { database : DatabaseConfig.Type, listen_addr : Text }
      , default.database = DatabaseConfig.default
      }

in  BaseConfig::{
    , database = DatabaseConfig::{
      , db_url = "postgres:///darkkirb?host=/run/postgresql"
      }
    , listen_addr = "[::1]:5621"
    }
