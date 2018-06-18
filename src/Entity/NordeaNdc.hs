module Entity.NordeaNdc
  ( contract
  ) where

import Protolude (Maybe(..), ($))
import Entity    (EntityContract(EntityContract), JweContract(..))
import Jose.Jwa  (JweAlg(..), Enc(..), JwsAlg(..))

contract :: EntityContract
contract = EntityContract RS256 $ Just $ JweContract RSA_OAEP A128GCM