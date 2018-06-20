module Conf
  ( Environment(..)
  , confFileName
  , KeyRing(..)
  , logger
  ) where

import Protolude      (show, flip, (++), Read, Show, (.))
import Data.Text.Lazy (toLower, pack, Text)
import Entity         (Speedledger(..), Nordea(..))
import Network.Wai.Middleware.RequestLogger
import Network.Wai    (Middleware)

data Environment
  = Development
  | Staging
  | Production
  deriving (Read, Show)

logger :: Environment -> Middleware
logger Production = logStdout
logger _          = logStdoutDev

confFileName :: Environment -> Text
confFileName = toLower . pack . flip (++) ".conf" . show

data KeyRing
  = KeyRing
  { speedledger :: Speedledger
  , nordea      :: Nordea
  }