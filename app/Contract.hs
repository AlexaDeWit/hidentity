module Contract
  ( Contract(..)
  , Channel(..)
  , JweContract(..)
  , Keyable(..)
  , symmetricalChannel
  ) where

import Protolude (Maybe)
import Jose.Jwk  (Jwk)
import Jose.Jwa  (JweAlg, Enc, JwsAlg)

data JweContract
  = JweContract JweAlg Enc

class Keyable a where
  retrieveKey :: a -> Jwk

data (Keyable s, Keyable r) => Contract s r
  = Contract
  { jwsAlg      :: JwsAlg
  , jweContract :: Maybe JweContract
  , sender      :: s
  , recipient   :: r
  }

data (Keyable e1, Keyable e2) => Channel e1 e2
 = Channel
  { outGoing :: Contract e1 e2
  , inComing :: Contract e2 e1
  }

symmetricalChannel :: (Keyable sender, Keyable recipient) => sender -> recipient -> JwsAlg -> Maybe JweContract -> Channel sender recipient
symmetricalChannel s r alg jwec = Channel sendingContract receivingContract where
    sendingContract   = Contract alg jwec s r
    receivingContract = Contract alg jwec r s

