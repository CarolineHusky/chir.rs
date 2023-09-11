module Main where

import Config qualified
import Control.Lens (makeLenses)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend, runSqlPool)
import Database.Persist.Sql.Migration (runMigration)
import Database.Persist.Mixed (create)
import Database.Persist.Model (migrateAll)
import Main.Utf8 qualified as Utf8
import Yesod (
  HandlerFor,
  Html,
  RenderRoute (renderRoute),
  Yesod (defaultLayout),
  mkYesod,
  parseRoutes,
  warp,
  whamlet,
 )

newtype ChirRs = ChirRs
  { _dbPool :: Pool SqlBackend
  }

makeLenses ''ChirRs

mkYesod
  "ChirRs"
  [parseRoutes|
/ HomeR GET
|]

instance Yesod ChirRs
getHomeR :: HandlerFor ChirRs Html
getHomeR = defaultLayout [whamlet|Hewo!|]

{- |
 Main entry point.

 The `, run` script will invoke this function.
-}
main :: IO ()
main = do
  -- For withUtf8, see https://serokell.io/blog/haskell-with-utf8
  Utf8.withUtf8 $ do
    config <- runStderrLoggingT Config.loadConfigAuto
    print config
    pool <- runStderrLoggingT $ create config
    runSqlPool (runMigration migrateAll) pool
    warp (fromIntegral $ Config.listenPort config) $ ChirRs pool
