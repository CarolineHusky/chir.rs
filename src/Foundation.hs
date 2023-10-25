{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fmap" #-}

module Foundation (
  App (..),
  Route (..),
  appConfig,
  appLogger,
  appStatic,
  appHttpManager,
  resourcesApp,
  Widget,
  QueueCommands (..),
  newRequest,
  returnJSON,
  requireJSONBody,
) where

import Codec.CBOR.JSON (decodeValue, encodeValue)
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Write (toLazyByteString)
import Codec.Serialise (Serialise (encode))
import Codec.Serialise.Class (Serialise (decode))
import Codec.Serialise.Decoding (Decoder, decodeInt, decodeListLen, decodeWord, decodeWord8)
import Codec.Serialise.Encoding (Encoding, encodeInt, encodeListLen, encodeString, encodeWord, encodeWord8)
import Conduit (connect, sinkLazy)
import Config (ConfigFile, logLevel', rpId', staticDir', toLogLevel, widgetFile)
import Config.StaticFiles (index_css, index_js)
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad (liftM)
import Control.Monad.Logger (LogLevel, LogSource)
import Crypto.JOSE (Crv (..), KeyMaterialGenParam (..))
import Crypto.JOSE.JWA.JWK (OKPCrv (..))
import Data.Aeson qualified as A
import Data.ByteString qualified as B8
import Database.Persist.Postgresql (SqlPersistT)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Database.Persist.SqlBackend (SqlBackend)
import Network.HTTP.Client.Conduit (Manager, Request (requestHeaders), parseRequest_)
import Network.HTTP.Types (hUserAgent)
import Text.Hamlet (hamletFile)
import Text.Internationalisation
import Text.Jasmine (minifym)
import Utils (headOr)
import Yesod (
  DBRunner,
  FormMessage,
  Html,
  Lang,
  MonadHandler,
  PageContent (pageBody, pageHead, pageTitle),
  RenderMessage,
  RenderRoute (Route, renderRoute),
  SessionBackend,
  ToContent (toContent),
  ToTypedContent,
  TypedContent (TypedContent),
  Yesod (
    addStaticContent,
    defaultLayout,
    makeLogger,
    makeSessionBackend,
    shouldLogIO,
    yesodMiddleware
  ),
  YesodPersist (runDB),
  YesodPersistRunner,
  YesodRequest (reqAccept),
  addHeader,
  addScript,
  addStylesheet,
  defaultFormMessage,
  defaultGetDBRunner,
  getRequest,
  getYesod,
  invalidArgs,
  languages,
  lookupCookie,
  lookupHeader,
  mkYesodData,
  parseRoutesFile,
  rawRequestBody,
  requireCheckJsonBody,
  widgetToPageContent,
  withUrlRenderer,
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
  , _appHttpManager :: Manager
  -- ^ HTTP client
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
  yesodMiddleware handler = do
    app <- getYesod
    -- only allow local source, no embeds
    addHeader "X-Frame-Options" "DENY"
    addHeader "X-XSS-Protection" "0"
    addHeader "X-Content-Type-Options" "nosniff"
    addHeader "Referrer-Policy" "strict-origin-when-cross-origin"
    addHeader "Content-Security-Policy" "default-src 'self'; img-src 'self', data: ; frame-ancestors 'none'; upgrade-insecure-requests; block-all-mixed-content; disown-opener; base-uri 'self'"
    addHeader "Cross-Origin-Opener-Policy" "same-origin"
    addHeader "Cross-Origin-Embedder-Policy" "require-corp"
    addHeader "Cross-Origin-Resource-Policy" "same-site"
    addHeader "Permissions-Policy" "publickey-credentials-create=*, publickey-credentials-get=*, interest-cohort=()"
    addHeader "Link" $ "<https://" <> app ^. appConfig . rpId' <> "/.well-known/openid-configuration>; rel=\"indieauth-metadata\""
    handler

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
  shouldLogIO app _source level = return $ toLogLevel (app ^. (appConfig . logLevel')) <= level

  makeLogger :: App -> IO Logger
  makeLogger app = return $ app ^. appLogger

  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    themeCookie <- lookupCookie "_THEME"
    let theme = fromMaybe "" themeCookie
    langs <- languages
    let lang = headOr langs "en"

    pc <- widgetToPageContent $ do
      addScript $ StaticR index_js
      addStylesheet $ StaticR index_css
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

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

data QueueCommands
  = Rekey Text KeyMaterialGenParam Int
  | RefetchFidoMetadata
  deriving stock (Show, Eq)

instance Serialise QueueCommands where
  encode :: QueueCommands -> Encoding
  encode (Rekey name param nextRekey) = encodeListLen 4 <> encodeWord 0 <> encodeString name <> encode param <> encodeInt nextRekey
  encode RefetchFidoMetadata = encodeListLen 1 <> encodeWord 1

  decode :: Decoder s QueueCommands
  decode = do
    length' <- decodeListLen
    tag <- decodeWord
    case (length', tag) of
      (4, 0) -> Rekey <$> decode <*> decode <*> decode
      (1, 1) -> pure RefetchFidoMetadata
      _ -> fail "Invalid QueueCommands encoding"

newRequest :: String -> Request
newRequest s =
  let request = parseRequest_ s
   in request {requestHeaders = (hUserAgent, "Mozilla/5.0 (Compatible; chir.rs)") : requestHeaders request}

instance Serialise KeyMaterialGenParam where
  encode :: KeyMaterialGenParam -> Encoding
  encode (ECGenParam P_256) = encodeListLen 1 <> encodeWord8 0
  encode (ECGenParam P_384) = encodeListLen 1 <> encodeWord8 1
  encode (ECGenParam P_521) = encodeListLen 1 <> encodeWord8 2
  encode (ECGenParam Secp256k1) = encodeListLen 1 <> encodeWord8 3
  encode (RSAGenParam len) = encodeListLen 2 <> encodeWord8 4 <> encodeInt len
  encode (OctGenParam len) = encodeListLen 2 <> encodeWord8 5 <> encodeInt len
  encode (OKPGenParam Ed25519) = encodeListLen 1 <> encodeWord8 6
  encode (OKPGenParam Ed448) = encodeListLen 1 <> encodeWord8 7
  encode (OKPGenParam X25519) = encodeListLen 1 <> encodeWord8 8
  encode (OKPGenParam X448) = encodeListLen 1 <> encodeWord8 9

  decode :: Decoder s KeyMaterialGenParam
  decode = do
    length' <- decodeListLen
    tag <- decodeWord8
    case (length', tag) of
      (1, 0) -> pure $ ECGenParam P_256
      (1, 1) -> pure $ ECGenParam P_384
      (1, 2) -> pure $ ECGenParam P_521
      (1, 3) -> pure $ ECGenParam Secp256k1
      (2, 4) -> decodeInt <&> RSAGenParam
      (2, 5) -> decodeInt <&> OctGenParam
      (1, 6) -> pure $ OKPGenParam Ed25519
      (1, 7) -> pure $ OKPGenParam Ed448
      (1, 8) -> pure $ OKPGenParam X25519
      (1, 9) -> pure $ OKPGenParam X448
      _ -> fail "Invalid KeyMaterialGenParam encoding"

acceptsCbor :: (MonadHandler m) => m Bool
acceptsCbor =
  ( maybe False ((== "application/cbor") . B8.takeWhile (/= 0x3b))
      . listToMaybe
      . reqAccept
  )
    `liftM` getRequest

returnJSON :: (MonadHandler m, A.ToJSON a) => a -> m TypedContent
returnJSON a =
  acceptsCbor >>= \case
    False -> return $ TypedContent "application/json" $ toContent $ A.encode a
    True -> return $ TypedContent "application/cbor" $ toContent $ toLazyByteString $ encodeValue $ A.toJSON a

requireJSONBody :: (MonadHandler m, A.FromJSON a) => m a
requireJSONBody = do
  mct <- fromMaybe "" <$> lookupHeader "content-type"
  case mct of
    "application/json" -> requireCheckJsonBody
    "application/cbor" -> do
      bodyLBS <- connect rawRequestBody sinkLazy
      case deserialiseFromBytes (decodeValue False) bodyLBS of
        Left _ -> invalidArgs ["body"]
        Right (_, v) -> case A.fromJSON v of
          A.Error _ -> invalidArgs ["body"]
          A.Success v' -> return v'
    _ -> invalidArgs ["body"]
