module Entity.NordeaNdc
  ( channel
  ) where

import Protolude (Maybe(..), ($))
import Entity
import Jose.Jwa  (JweAlg(..), Enc(..), JwsAlg(..))

channel :: Speedledger -> Nordea -> Channel Speedledger Nordea
channel sl ndc = symmetricalChannel sl ndc RS256 $ Just $ JweContract RSA_OAEP_256 A128GCM