module Network.Webfinger.Link where

import Control.Lens (makeLensesFor)
import Data.Aeson (Options (fieldLabelModifier, omitNothingFields), defaultOptions)
import Data.Aeson.TH (deriveJSON)

data WebfingerLink = WebfingerLink
  { _rel :: Text
  , _type :: Maybe Text
  , _href :: Text
  , _titles :: Maybe (Map Text Text)
  , _properties :: Maybe (Map Text (Maybe Text))
  }

makeLensesFor
  [ ("_rel", "rel")
  , ("_type", "lType")
  , ("_href", "href")
  , ("_titles", "titles")
  , ("_properties", "properties")
  ]
  ''WebfingerLink
deriveJSON defaultOptions {fieldLabelModifier = drop 1, omitNothingFields = True} ''WebfingerLink
