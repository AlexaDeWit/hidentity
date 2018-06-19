module Entity
  ( Nordea(..)
  , Speedledger(..)
  ) where

import Protolude (Show)
import Jose.Jwk  (Jwk)
import Contract  (Keyable(..))

data Nordea
 = Nordea
 { nordeaKey :: Jwk
 }
 deriving (Show)

instance Keyable Nordea where
  retrieveKey = nordeaKey

data Speedledger
 = Speedledger
 { slKey :: Jwk
 }
 deriving (Show)

instance Keyable Speedledger where
  retrieveKey = slKey

