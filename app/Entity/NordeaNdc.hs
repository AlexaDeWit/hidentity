module Entity.NordeaNdc
  ( channel
  ) where

import Protolude (($))
import Entity
import Contract  (Channel(..), PayloadFormat(..), symmetricalChannel)
import Jose.Jwa  (JweAlg(..), Enc(..), JwsAlg(..))

channel :: Speedledger -> Nordea -> Channel Speedledger Nordea
channel sl ndc = symmetricalChannel sl ndc $ WrappedJwt RS256 RSA_OAEP_256 A128GCM