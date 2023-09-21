module Model.Migration (migration) where

import Database.Persist.Migration (
  Column (Column),
  ColumnProp (AutoIncrement, Default, NotNull, References),
  MigrateSql (MigrateSql),
  Migration,
  MigrationPath ((:=)),
  Operation (CreateTable, RawOperation, constraints, message, name, rawOp, schema),
  PersistValue (PersistInt64),
  SqlType (SqlBlob, SqlBool, SqlDayTime, SqlInt64, SqlString),
  TableConstraint (PrimaryKey),
  (~>),
 )

createLocalAccount :: Operation
createLocalAccount =
  CreateTable
    { name = "local_account"
    , schema =
        [ Column "username" SqlString [NotNull]
        , Column "password_hash" SqlString [NotNull]
        , Column "enabled" SqlBool [NotNull]
        ]
    , constraints =
        [ PrimaryKey ["username"]
        ]
    }

createLocalAccountCredentials :: Operation
createLocalAccountCredentials =
  CreateTable
    { name = "local_account_credentials"
    , schema =
        [ Column "id" SqlInt64 [NotNull, AutoIncrement]
        , Column "credential_id" SqlBlob [NotNull]
        , Column "user" SqlString [NotNull, References ("local_account", "username")]
        , Column "public_key" SqlBlob [NotNull]
        , Column "sign_counter" SqlInt64 [NotNull]
        , Column "transports" SqlBlob [NotNull]
        ]
    , constraints =
        [ PrimaryKey ["id"]
        ]
    }

createLocalAccountSessions :: Operation
createLocalAccountSessions =
  CreateTable
    { name = "local_account_sessions"
    , schema =
        [ Column "user" SqlString [NotNull, References ("local_account", "username")]
        , Column "jid" SqlString [NotNull]
        ]
    , constraints =
        [ PrimaryKey ["jid"]
        ]
    }

createWebFingerAccount :: Operation
createWebFingerAccount =
  CreateTable
    { name = "web_finger_account"
    , schema =
        [ Column "webfinger_username" SqlString [NotNull]
        , Column "indie_username" SqlString [NotNull]
        , Column "oidc_issuer" SqlString []
        ]
    , constraints =
        [ PrimaryKey ["webfinger_username"]
        ]
    }

createJobs :: Operation
createJobs =
  CreateTable
    { name = "jobs"
    , schema =
        [ Column "id" SqlInt64 [NotNull, AutoIncrement]
        , Column "created_at" SqlDayTime [NotNull]
        , Column "updated_at" SqlDayTime [NotNull]
        , Column "run_at" SqlDayTime [NotNull]
        , Column "payload" SqlBlob [NotNull]
        , Column "last_error" SqlBlob [NotNull]
        , Column "attempts" SqlInt64 [NotNull, Default $ PersistInt64 0]
        , Column "locked" SqlBool [NotNull]
        , Column "locked_at" SqlDayTime []
        , Column "locked_by" SqlString []
        ]
    , constraints =
        [ PrimaryKey ["id"]
        ]
    }

createKeys :: Operation
createKeys =
  CreateTable
    { name = "keys"
    , schema =
        [ Column "name" SqlString [NotNull]
        , Column "jwk" SqlBlob [NotNull]
        ]
    , constraints =
        [ PrimaryKey ["name"]
        ]
    }

migration :: Migration
migration =
  [ 0
      ~> 1
      := [ createLocalAccount
         , createLocalAccountCredentials
         , createLocalAccountSessions
         , createWebFingerAccount
         ]
  , 1
      ~> 2
      := [ createJobs
         , RawOperation
            { message = "Add indexes to Jobs"
            , rawOp =
                return
                  [ MigrateSql "CREATE INDEX jobs_created_at ON jobs (created_at)" []
                  , MigrateSql "CREATE INDEX jobs_updated_at ON jobs (updated_at)" []
                  , MigrateSql "CREATE INDEX jobs_run_at ON jobs (run_at)" []
                  , MigrateSql "CREATE INDEX jobs_locked ON jobs (locked)" []
                  ]
            }
         ]
  , 2 ~> 3 := [createKeys]
  ]
