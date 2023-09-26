module Model.Migration (migration) where

import Data.Time (getCurrentTime)
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import Database.Persist.Migration (
  Column (Column),
  ColumnProp (AutoIncrement, Default, NotNull, References),
  MigrateSql (MigrateSql),
  Migration,
  MigrationPath ((:=)),
  Operation (CreateTable, DropColumn, RawOperation, constraints, message, name, rawOp, schema),
  PersistValue (PersistInt64, PersistUTCTime),
  SqlType (SqlBlob, SqlBool, SqlDayTime, SqlInt64, SqlString),
  TableConstraint (PrimaryKey, Unique),
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

createWebauthnChallenge :: Operation
createWebauthnChallenge =
  CreateTable
    { name = "webauthn_challenge"
    , schema =
        [ Column "jti" SqlString [NotNull]
        , Column "expires_at" SqlDayTime [NotNull]
        ]
    , constraints =
        [ PrimaryKey ["jti"]
        ]
    }

createKeyValueBlob :: Operation
createKeyValueBlob =
  CreateTable
    { name = "key_value_blob"
    , schema =
        [ Column "key" SqlString [NotNull]
        , Column "value" SqlBlob [NotNull]
        ]
    , constraints = [PrimaryKey ["key"]]
    }

createLocalAccountSessionScopes :: Operation
createLocalAccountSessionScopes =
  CreateTable
    { name = "local_account_session_scopes"
    , schema =
        [ Column "id" SqlInt64 [NotNull, AutoIncrement]
        , Column "jid" SqlString [NotNull, References ("local_account_sessions", "jid")]
        , Column "scope" SqlString [NotNull]
        ]
    , constraints = [PrimaryKey ["id"], Unique "unique_local_account_session_scopes" ["jid", "scope"]]
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
  , 3 ~> 4 := [createWebauthnChallenge]
  , 4 ~> 5 := [DropColumn ("local_account", "password_hash")]
  , 5 ~> 6 := [createKeyValueBlob]
  , 6
      ~> 7
      := [ RawOperation
            { message = "change local_account_sessions pkey"
            , rawOp = do
                now <- liftIO getCurrentTime
                let expTime = addUTCTime (604_800 :: NominalDiffTime) now
                return
                  [ MigrateSql "ALTER TABLE local_account_sessions DROP CONSTRAINT local_account_sessions_pkey, ADD PRIMARY KEY (jid)" []
                  , MigrateSql "CREATE INDEX local_account_sessions_user ON local_account_sessions (\"user\")" []
                  , MigrateSql "ALTER TABLE local_account_sessions ADD COLUMN last_access TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()" []
                  , MigrateSql "ALTER TABLE local_account_sessions ADD COLUMN until TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT ?" [PersistUTCTime expTime]
                  , MigrateSql "ALTER TABLE local_account_sessions ALTER COLUMN last_access DROP DEFAULT" []
                  , MigrateSql "ALTER TABLE local_account_sessions ALTER COLUMN until DROP DEFAULT" []
                  ]
            }
         , createLocalAccountSessionScopes
         ]
  ]
