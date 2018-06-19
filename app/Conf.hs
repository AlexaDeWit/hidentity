module Conf
  ( Environment(..)
  , confFileName
  , KeyRing(..)
  ) where

import Protolude      (show, flip, (++), Read, Show, (.))
import Data.Text.Lazy (toLower, pack, Text)
import Entity         (Speedledger(..), Nordea(..))

data Environment
  = Development
  | Staging
  | Production
  deriving (Read, Show)


confFileName :: Environment -> Text
confFileName = toLower . pack . flip (++) ".conf" . show

data KeyRing
  = KeyRing
  { speedledger :: Speedledger
  , nordea      :: Nordea
  }