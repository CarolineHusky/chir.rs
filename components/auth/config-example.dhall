let BaseConfig =
      { Type =
          { database_url : Text
          , listen_addr : Text
          , redis_url : Text
          , asset_path : Text
          }
      , default.listen_addr = "[::1]:5621"
      }

in  BaseConfig::{
    , database_url = "postgres:///darkkirb?host=/run/postgresql&user=darkkirb"
    , redis_url = "redis://localhost/0"
    , asset_path = "../../result"
    }
