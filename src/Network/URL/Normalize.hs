-- | Functions for normalizing URLs into the format expected by indieweb applications
module Network.URL.Normalize where

import Data.Char (toLower)
import Data.Text qualified as T
import Network.URL (URL)
import Network.URL qualified as URL

importURL :: Text -> Maybe URL
importURL = URL.importURL . toString

exportURL :: URL -> Text
exportURL = toText . URL.exportURL

normalizeURL :: Text -> Maybe Text
normalizeURL text =
  if not ('/' `T.elem` text) && text /= ""
    then Just ("https://" <> text <> "/")
    else do
      url@URL.URL
        { URL.url_type =
          URL.Absolute
            URL.Host
              { URL.host = hostname
              , URL.protocol = URL.HTTP True
              , URL.port = Nothing
              }
        } <-
        importURL text
      if hostname == "" then mempty else pass
      let new_url =
            URL.URL
              { URL.url_type =
                  URL.Absolute
                    URL.Host
                      { URL.host = toLower <$> hostname
                      , URL.protocol = URL.HTTP True
                      , URL.port = Nothing
                      }
              , URL.url_path = URL.url_path url
              , URL.url_params = URL.url_params url
              }
      pure $ exportURL new_url