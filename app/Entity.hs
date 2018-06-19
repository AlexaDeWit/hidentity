module Entity
  ( JweContract(..)
  , Contract(..)
  , Channel(..)
  , Nordea(..)
  , Speedledger(..)
  , symmetricalChannel
  ) where

import Protolude (Maybe, Show)
import Jose.Jwa  (JweAlg, Enc, JwsAlg)
import Jose.Jwk  (Jwk)

data Contract s r
  = Contract
  { jwsAlg      :: JwsAlg
  , jweContract :: Maybe JweContract
  , sender      :: s
  , recipient   :: r
  }

data JweContract
  = JweContract JweAlg Enc

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

data Channel e1 e2
 = Channel
  { outGoing :: Contract e1 e2
  , inComing :: Contract e2 e1
  }

symmetricalChannel :: sender -> recipient -> JwsAlg -> Maybe JweContract -> Channel sender recipient
symmetricalChannel s r alg jwec = Channel sendingContract receivingContract where
    sendingContract   = Contract alg jwec s r
    receivingContract = Contract alg jwec r s
