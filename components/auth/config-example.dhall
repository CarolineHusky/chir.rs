let BaseConfig =
      { Type =
          { database_url : Text
          , listen_addr : Text
          , redis_url : Text
          }
        , default.listen_addr = "[::1]:5621"
      }

in  BaseConfig::{
    , database_url = "postgres:///darkkirb?host=/run/postgresql&user=darkkirb"
    , listen_addr = "[::1]:5621"
    , redis_url = "redis://localhost/0"
    }
