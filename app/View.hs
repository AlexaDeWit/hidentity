module View
  ( parsedClaims
  ) where


import Protolude               (Either(..), ($), panic)
import Web.Scotty              (ActionM(..))
import Contract                (ClaimsBlob(..), PayloadError(..))
import Data.Text               (unpack, pack, append)

parsedClaims :: Either PayloadError ClaimsBlob -> ActionM ()
parsedClaims = panic $ pack $ "Lol"