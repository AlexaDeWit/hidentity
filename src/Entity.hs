module Entity
  ( JweContract(..)
  , EntityContract(..)
  ) where

import Protolude (Maybe)
import Jose.Jwa  (JweAlg, Enc, JwsAlg)

data EntityContract
  = EntityContract
  { jwsAlg      :: JwsAlg
  , jweContract :: Maybe JweContract
  }

data JweContract
  = JweContract JweAlg Enc