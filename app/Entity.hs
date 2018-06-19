module Entity
  ( Nordea(..)
  , Speedledger(..)
  ) where

import Protolude (Show)
import Jose.Jwk  (Jwk)
import Contract  (Contract(..), Channel(..))

data Nordea
 = Nordea
 { nordeaKey :: Jwk
 }
 deriving (Show)

data Speedledger
 = Speedledger
 { slKey :: Jwk
 }
 deriving (Show)

