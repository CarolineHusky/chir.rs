module Model.Migration where

import Database.Persist.Migration (
  Column (Column),
  ColumnProp (AutoIncrement, NotNull, References),
  Migration,
  MigrationPath ((:=)),
  Operation (CreateTable, constraints, name, schema),
  SqlType (SqlBlob, SqlBool, SqlInt64, SqlString),
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

migration :: Migration
migration =
  [ 0
      ~> 1
      := [ createLocalAccount
         , createLocalAccountCredentials
         , createLocalAccountSessions
         , createWebFingerAccount
         ]
  ]
