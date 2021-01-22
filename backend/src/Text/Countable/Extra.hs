module Text.Countable.Extra where

import Data.Text (Text)
import qualified Data.Text as Text
import Text.Countable (singularize)

singularizeWords :: Text -> [Text]
singularizeWords = fmap singularize . Text.words
