module Database.Persist.Model where

import Database.Persist.TH (
  mkMigrate,
  mkPersist,
  persistLowerCase,
  share,
  sqlSettings,
 )

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
LocalAccount
    username Text
    password_hash Text
    deriving Show
LocalAccountCredentials
    credentialId ByteString
    user LocalAccountId
    publicKey ByteString
    signCounter Int
    transports ByteString
    deriving Show
LocalAccountSessions
    user LocalAccountId
    jid Text
    deriving Show
|]