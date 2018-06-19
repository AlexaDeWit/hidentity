module Conf
  ( Environment(..)
  , confFileName
  ) where

import Protolude      (show, flip, (++), Read, Show, (.))
import Data.Text.Lazy (toLower, pack, Text)

data Environment
  = Development
  | Staging
  | Production
  deriving (Read, Show)


confFileName :: Environment -> Text
confFileName = toLower . pack . flip (++) ".conf" . show
