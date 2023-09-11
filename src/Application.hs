module Application (appMain) where

import Config (ConfigFile, listenPort', loadConfigAuto, staticDir')
import Control.Lens ((^.))
import Control.Monad.Logger (LogLevel (LevelError), LoggingT (runLoggingT), liftLoc, runStderrLoggingT)
import Data.Default (def)
import Database.Persist.Mixed (createConn)
import Database.Persist.Sql (runSqlPool)
import Database.Persist.Sql.Migration (runMigration)
import Foundation (
  App (..),
  Route (HomeR, StaticR),
  appConfig,
  appLogger,
  appStatic,
  resourcesApp,
 )
import Handler.Home (getHomeR)
import Language.Haskell.TH.Syntax (qLocation)
import Model (migrateAll)
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
import Yesod.Default.Config2 (makeYesodLogger)
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
  pool <- flip runLoggingT logFunc $ createConn config

  -- Perform database migration using our application's logging settings.
  runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

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
