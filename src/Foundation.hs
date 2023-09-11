module Foundation (
  App (..),
  Route (HomeR, StaticR),
  appConfig,
  appLogger,
  appStatic,
  resourcesApp,
) where

import Config (ConfigFile, logLevel', staticDir', toLogLevel)
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad.Logger (LogLevel, LogSource)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Database.Persist.SqlBackend (SqlBackend)
import Database.Persist.Sqlite (SqlPersistT)
import Text.Jasmine (minifym)
import Yesod (
  DBRunner,
  FormMessage,
  Lang,
  RenderMessage,
  RenderRoute (Route, renderRoute),
  SessionBackend,
  ToTypedContent,
  Yesod (addStaticContent, makeLogger, makeSessionBackend, shouldLogIO, yesodMiddleware),
  YesodPersist (runDB),
  YesodPersistRunner,
  defaultFormMessage,
  defaultGetDBRunner,
  defaultYesodMiddleware,
  getYesod,
  mkYesodData,
  parseRoutesFile,
 )
import Yesod.Core (RenderMessage (renderMessage))
import Yesod.Core.Types (Logger)
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Persist (YesodPersistBackend)
import Yesod.Persist.Core (YesodPersistRunner (getDBRunner))
import Yesod.Static (Route (StaticRoute), Static, base64md5)

data App = App
  { _appConfig :: ConfigFile
  -- ^ Configuration file
  , _appDbPool :: ConnectionPool
  -- ^ Database pool
  , _appStatic' :: Static
  -- ^ Static content
  , _appLogger :: Logger
  -- ^ Logger
  }

makeLenses ''App

appStatic :: App -> Static
appStatic = flip (^.) appStatic'

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

instance Yesod App where
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ = return Nothing

  yesodMiddleware :: (ToTypedContent res) => Handler res -> Handler res
  yesodMiddleware = defaultYesodMiddleware

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent ::
    Text ->
    -- \^ The file extension
    Text ->
    -- \^ The MIME content type
    LByteString ->
    -- \^ The contents of the file
    Handler (Maybe (Either Text (Route App, [(Text, Text)])))
  addStaticContent ext mime content = do
    app <- getYesod
    let staticDir = toString $ app ^. (appConfig . staticDir')
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
      -- Generate a unique filename based on the content itself
      genFileName lbs = "autogen-" ++ base64md5 lbs

  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level = return $ toLogLevel (app ^. (appConfig . logLevel')) >= level

  makeLogger :: App -> IO Logger
  makeLogger app = return $ app ^. appLogger

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB :: SqlPersistT Handler a -> Handler a
  runDB action = do
    app <- getYesod
    runSqlPool action $ app ^. appDbPool

instance YesodPersistRunner App where
  getDBRunner :: Handler (DBRunner App, Handler ())
  getDBRunner = defaultGetDBRunner $ flip (^.) appDbPool

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage
