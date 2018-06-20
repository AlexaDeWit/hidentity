module View
  ( parsedClaims
  ) where


import Protolude               (Either(..), ($), show, Int)
import Web.Scotty              (ActionM, json, status)
import Contract                (Claims, PayloadError(..))
import Data.Text               (pack, Text)
import Data.Text.Encoding      (encodeUtf8)
import Network.HTTP.Types.Status (Status(..))

statusWithText :: Int -> Text -> Status
statusWithText code text = Status code $ encodeUtf8 text

parsedClaims :: Either PayloadError Claims -> ActionM ()
parsedClaims payload =
  case payload of
    Left err                      -> status $ statusWithText 400 $ pack $ show err
    Right (claims)                -> json claims