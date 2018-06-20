module View
  ( parsedClaims
  ) where


import Protolude               (Either(..), ($), show, Int, take, fmap, any, Bool, identity)
import Web.Scotty              (ActionM, json, status, text)
import Contract                (Claims, PayloadError(..))
import Data.Text               (pack, Text, splitOn, intercalate, filter)
import Data.Char               (isSpace, isAlpha)
import Data.Text.Lazy          (fromStrict)
import Data.Text.Encoding      (encodeUtf8)
import Network.HTTP.Types.Status (Status(..))

formatErrorText :: Text -> Text
formatErrorText t = intercalate interspersionText firstTwo where
  cleaned = filter (anyTrue [isSpace, isAlpha]) t
  interspersionText = pack " "
  split = splitOn interspersionText cleaned
  firstTwo = take 2 split

anyTrue :: [a -> Bool] -> a -> Bool
anyTrue fs a = any identity $ fmap ($ a) fs

statusWithText :: Int -> Text -> ActionM ()
statusWithText code t = do
  _ <- status $ Status code $ encodeUtf8 $ formatErrorText t
  text $ fromStrict t

parsedClaims :: Either PayloadError Claims -> ActionM ()
parsedClaims payload =
  case payload of
    Left err                      -> statusWithText 400 $ pack $ show err
    Right (claims)                -> json claims