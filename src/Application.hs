module Application (appMain, develMain) where

import Config (ConfigFile (database, databasePoolSize), listenPort', loadConfigAuto, nodeName', staticDir', toPostgresConf)
import Control.Lens ((^.))
import Control.Monad.Logger (LogLevel (LevelError), LoggingT (runLoggingT), liftLoc, runStderrLoggingT)
import Crypto.KeyStore (performRekey)
import Data.Default (def)
import Data.Queue qualified as Queue
import Database.Persist.Migration qualified as DPM
import Database.Persist.Migration.Postgres (runMigration)
import Database.Persist.Postgresql (createPostgresqlPoolWithConf, defaultPostgresConfHooks)
import Database.Persist.Sql (runSqlPool)
import Foundation (
  App (..),
  QueueCommands (Rekey),
  Route (..),
  appConfig,
  appLogger,
  appStatic,
  resourcesApp,
 )
import Handler.Home (getHomeR)
import Handler.Webfinger (getWebfingerR)
import Language.Haskell.TH.Syntax (qLocation)
import Model (migrateAll)
import Model.Migration (migration)
import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (Settings, defaultSettings, defaultShouldDisplayException, runSettings, setOnException, setPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger), OutputFormat (Detailed), RequestLoggerSettings (..), mkRequestLogger)
import System.Log.FastLogger (ToLogStr (toLogStr), defaultBufSize, newStdoutLoggerSet)
import Yesod (
  Yesod (messageLoggerSource),
  defaultMiddlewaresNoLogging,
  mkYesodDispatch,
  toWaiAppPlain,
 )
import Yesod.Core.Types (Logger (loggerSet))
import Yesod.Default.Config2 (develMainHelper, getDevSettings, makeYesodLogger)
import Yesod.Static (static)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

{- | This function allocates resources (such as a database connection pool),
performs initialization and returns a foundation datatype value. This is also
the place to put your migrate statements to have automatic database
migrations handled by Yesod.
-}
makeFoundation :: ConfigFile -> IO App
makeFoundation config = do
  appLogger' <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
  appStatic' <- static $ toString $ config ^. staticDir'
  -- We need a log function to create a connection pool. We need a connection
  -- pool to create our foundation. And we need our foundation to get a
  -- logging function. To get out of this loop, we initially create a
  -- temporary foundation without a real connection pool, get a log function
  -- from there, and then create the real foundation.
  let mkFoundation appConnPool =
        App
          { _appConfig = config
          , _appDbPool = appConnPool
          , _appStatic' = appStatic'
          , _appLogger = appLogger'
          }
      -- The App {..} syntax is an example of record wild cards. For more
      -- information, see:
      -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
      tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
      logFunc = messageLoggerSource tempFoundation appLogger'
  pool <- flip runLoggingT logFunc $ createPostgresqlPoolWithConf (toPostgresConf (database config) (fromIntegral $ databasePoolSize config)) defaultPostgresConfHooks

  -- Perform database migration using our application's logging settings.

  runSqlPool
    ( do
        runMigration DPM.defaultSettings migration
        DPM.checkMigration migrateAll
    )
    pool

  let queue =
        Queue.Queue
          { Queue.queueDbPool = pool
          , Queue.queueHandler =
              ( \case
                  Rekey name parms days -> do
                    _ <- runSqlPool (performRekey name parms days) pool
                    return $ Right ()
              ) ::
                QueueCommands -> IO (Either () ())
          , Queue.queueNodeName = config ^. nodeName'
          }
  Queue.run queue
  -- Return the foundation
  return $ mkFoundation pool

{- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
applying some additional middlewares.
-}
makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
  -- Create the WAI application and apply middlewares
  appPlain <- toWaiAppPlain foundation
  return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
  mkRequestLogger
    def
      { outputFormat = Detailed True
      , destination = Logger $ loggerSet $ foundation ^. appLogger
      }

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
  setPort
    (fromIntegral $ foundation ^. (appConfig . listenPort'))
    $ setOnException
      ( \_req e ->
          when (defaultShouldDisplayException e) $
            messageLoggerSource
              foundation
              (foundation ^. appLogger)
              $(qLocation >>= liftLoc)
              "yesod"
              LevelError
              (toLogStr $ "Exception from Warp: " ++ show e)
      )
      defaultSettings

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
  config <- runStderrLoggingT loadConfigAuto
  foundation <- makeFoundation config
  -- Generate a WAI Application from the foundation
  app <- makeApplication foundation
  runSettings (warpSettings foundation) app

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
  config <- runStderrLoggingT loadConfigAuto
  foundation <- makeFoundation config
  wsettings <- getDevSettings $ warpSettings foundation
  app <- makeApplication foundation
  return (wsettings, app)

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev
