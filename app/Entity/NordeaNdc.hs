module Entity.NordeaNdc
  ( channel
  ) where

import Protolude (Maybe(..), ($))
import Entity
import Jose.Jwa  (JweAlg(..), Enc(..), JwsAlg(..))

channel :: Channel Speedledger Nordea
channel = symmetricalChannel Speedledger Nordea RS256 $ Just $ JweContract RSA_OAEP_256 A128GCM