module Conf
  ( ApplicationConfiguration(..)
  , confFileName
  ) where

import Protolude      (show, flip, (++), Read, Show, (.))
import Data.Text.Lazy (toLower, pack, Text)

data ApplicationConfiguration
  = Development
  | Staging
  | Production
  deriving (Read, Show)


confFileName :: ApplicationConfiguration -> Text
confFileName = toLower . pack . flip (++) ".conf" . show
