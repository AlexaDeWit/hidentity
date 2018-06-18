module Entity
  ( JweContract(..)
  , Contract(..)
  , Channel(..)
  , Nordea(..)
  , Speedledger(..)
  , symmetricalChannel
  ) where

import Protolude (Maybe)
import Jose.Jwa  (JweAlg, Enc, JwsAlg)

data Contract sender recipient
  = Contract
  { jwsAlg      :: JwsAlg
  , jweContract :: Maybe JweContract
  }

data JweContract
  = JweContract JweAlg Enc

data Nordea = Nordea

data Speedledger = Speedledger

data Channel e1 e2
 = Channel
  { outGoing :: Contract e1 e2
  , inComing :: Contract e2 e1
  }

symmetricalChannel :: entity1 -> entity2 -> JwsAlg -> Maybe JweContract -> Channel entity1 entity2
symmetricalChannel _ _ alg jwec = Channel contract contract where
    contract :: Contract e1 e2
    contract = Contract alg jwec
