module Network.Webfinger where

import Control.Lens (makeLenses)
import Data.Aeson (Options (fieldLabelModifier, omitNothingFields), defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Network.Webfinger.Link (WebfingerLink)

data Webfinger = Webfinger
  { _subject :: Text
  , _aliases :: Maybe [Text]
  , _properties :: Maybe (Map Text (Maybe Text))
  , _links :: Maybe [WebfingerLink]
  }

makeLenses ''Webfinger
deriveJSON defaultOptions {fieldLabelModifier = drop 1, omitNothingFields = True} ''Webfinger
