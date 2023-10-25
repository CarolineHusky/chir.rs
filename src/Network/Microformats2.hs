module Network.Microformats2 (fetchMF2) where

import Conduit (MonadThrow (throwM), MonadUnliftIO, connect)
import Control.Lens (makeLenses, (%~), (.~), (<>~), (^.))
import Control.Lens.At (at)
import Control.Monad.Trans.Resource (runResourceTChecked)
import Data.Aeson (Key, KeyValue, ToJSON (toJSON), Value, (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Lens (AsValue (_Object), atKey, key)
import Data.Aeson.Types (object)
import Data.Map qualified as Map
import Data.Microformats2.Parser qualified as MF2
import Data.Text (split)
import Foundation (newRequest)
import Network.HTTP.Conduit (Manager, Response (responseBody, responseHeaders, responseStatus), http)
import Network.HTTP.Link (Link, LinkParam (..), href, linkParams, parseLinkHeaderBS)
import Network.HTTP.Types (Header, Status, status200)
import Network.URI (parseURI)

newtype MicroformatsException = InvalidResponse Status
  deriving stock (Show)

instance Exception MicroformatsException

data LinkInfo = LinkInfo
  { _url :: Text
  , _rel :: [Text]
  , _hreflang :: [Text]
  , _media :: [Text]
  }

makeLenses ''LinkInfo

instance Semigroup LinkInfo where
  (<>) :: LinkInfo -> LinkInfo -> LinkInfo
  a <> b =
    a
      & url
      .~ (b ^. url)
        & rel
      %~ (<> b ^. rel)
        & hreflang
      %~ (<> b ^. hreflang)
        & media
      %~ (<> b ^. media)

instance Monoid LinkInfo where
  mempty :: LinkInfo
  mempty = baseUrl ""

baseUrl :: Text -> LinkInfo
baseUrl url' = LinkInfo url' [] [] []

applyParam :: LinkInfo -> LinkParam -> Text -> LinkInfo
applyParam v Rel rel' = v & rel %~ (split (== ' ') rel' ++)
applyParam v Anchor anchor = v & url %~ (<> ("#" <> anchor))
applyParam v Hreflang hreflang' = v & hreflang %~ (hreflang' :)
applyParam v Media media' = v & media %~ (media' :)
applyParam v _ _ = v

applyParams :: Text -> [(LinkParam, Text)] -> LinkInfo
applyParams = foldr (\(a, b) c -> applyParam c a b) . baseUrl

parseLinkInfo :: Link Text -> LinkInfo
parseLinkInfo link = applyParams (href link) (linkParams link)

parseLink :: ByteString -> [LinkInfo]
parseLink = (parseLinkInfo <$>) . fromMaybe [] . parseLinkHeaderBS

parseLinkHeaders :: [Header] -> [LinkInfo]
parseLinkHeaders = concatMap (parseLink . snd) . filter ((== "Link") . fst)

(.=?) :: (Data.Aeson.KeyValue kv, Data.Aeson.ToJSON a) => Data.Aeson.Key -> [a] -> Maybe kv
_ .=? [] = Nothing
k .=? v = Just (k Data.Aeson..= v)

linkInfoToRelUrl :: LinkInfo -> (Data.Aeson.Key, Data.Aeson.Value)
linkInfoToRelUrl info =
  ( fromText $ info ^. url
  , object $
      catMaybes
        [ "rel" .=? (info ^. rel)
        , "hreflang" .=? (info ^. hreflang)
        , "media" .=? (info ^. media)
        ]
  )

linkInfoToRelUrls :: [LinkInfo] -> KM.KeyMap Data.Aeson.Value
linkInfoToRelUrls = KM.fromList . map linkInfoToRelUrl

transmuteToRel' :: LinkInfo -> Map Text [Text] -> Map Text [Text]
transmuteToRel' LinkInfo {_rel = []} links = links
transmuteToRel' LinkInfo {_url = url', _rel = r} l =
  transmuteToRel'' l url' r
  where
    transmuteToRel'' links _ [] = links
    transmuteToRel'' links u (rel' : rels) =
      transmuteToRel'' links u rels
        & at rel'
        %~ Just
          . fromMaybe []
          & at rel'
        %~ ((u :) <$>)

transmuteToRel :: [LinkInfo] -> Map Text [Text]
transmuteToRel = foldr transmuteToRel' Map.empty

linkInfoToRels :: [LinkInfo] -> KM.KeyMap Data.Aeson.Value
linkInfoToRels = KM.fromList . map (bimap fromText Data.Aeson.toJSON) . Map.toList . transmuteToRel

fetchMF2 :: (MonadThrow m, MonadUnliftIO m) => Manager -> Text -> m Data.Aeson.Value
fetchMF2 manager url' = runResourceTChecked $ do
  let uri = parseURI $ toString url'
  let request = newRequest $ toString url'
  res <- http request manager
  if responseStatus res /= status200 then throwM $ InvalidResponse $ responseStatus res else pass
  let links = parseLinkHeaders $ responseHeaders res
  let relUrls = linkInfoToRelUrls links
  let rels = linkInfoToRels links
  mf2' <-
    MF2.parseMf2
      ( MF2.Mf2ParserSettings
          { MF2.htmlMode = MF2.Sanitize
          , MF2.baseUri = uri
          }
      )
      . MF2.documentRoot
      <$> connect (responseBody res) MF2.sinkDoc

  return $
    mf2'
      & atKey "rels"
      %~ Just
        . fromMaybe (object [])
        & key "rels"
        . _Object
      <>~ rels
        & atKey "rel-urls"
      %~ Just
        . fromMaybe (object [])
        & key "rel-urls"
        . _Object
      <>~ relUrls
