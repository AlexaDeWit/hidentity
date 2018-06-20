{-# LANGUAGE OverloadedStrings #-}

module App
 ( app
 ) where


import Protolude               (($), IO, (>>=), liftA2, putStrLn, return, (<&>), Either(..), liftIO)
import Data.String             (String)
import Data.ByteString.Lazy    (ByteString, toStrict)
import Web.Scotty
import Data.Either.Combinators (maybeToRight)
import Data.Text.Lazy          (unpack, pack, append)

import Conf
import Entity                  (Speedledger(..), Nordea(..))
import Contract                (retrievePayload, encodePayload, Direction(..), Claims, PayloadError(..), Channel, Keyable)

import qualified Data.Aeson              as Aeson
import qualified Data.Configurator       as C
import qualified Data.Configurator.Types as C

import qualified Entity.NordeaNdc        as Ndc
import qualified View                    as View
import qualified Jose.Jwt                as Jwt

app :: Environment -> IO ()
app env = do
  conf <- C.load [C.Required $ unpack $ confFileName env]
  maybeKeyRing <- makeKeyRing conf
  case maybeKeyRing of
    Left errText      -> putStrLn $ append (pack "Keyring could not be loaded, reason: ") (pack errText)
    Right keyRing -> do
      let ndcChannel = Ndc.channel (speedledger keyRing) (nordea keyRing)
      scotty 3000 $ do
        post "/nordea/validate" $ do
          claims <- getClaimsFromBody ndcChannel
          View.parsedClaims claims

        post "/nordea/encode" $ do
          jwt <- getJwePayloadFromBody ndcChannel
          View.encodedJwe jwt




makeKeyRing :: C.Config -> IO (Either String KeyRing)
makeKeyRing conf = do
  slKeyText     <- C.lookup conf "IdentityKeys.Speedledger"
  nordeaKeyText <- C.lookup conf "IdentityKeys.Ndc"
  let slKeyStr = maybeToRight "Could not load Speedledger key" slKeyText :: Either String ByteString
  let ndcKeyStr = maybeToRight "Could not load Nordea key" nordeaKeyText :: Either String ByteString
  let slKeyEither = slKeyStr >>= Aeson.eitherDecode <&> Speedledger
  let ndcKeyEither = ndcKeyStr >>= Aeson.eitherDecode <&> Nordea
  return $ liftA2 KeyRing slKeyEither ndcKeyEither

getClaimsFromBody
  :: (Keyable s, Keyable r)
  => Channel s r
  -> ActionM (Either PayloadError Claims)
getClaimsFromBody channel = do
  payload <- body
  liftIO $ retrievePayload channel Incoming $ toStrict payload

getJwePayloadFromBody
  :: (Keyable s, Keyable r)
  => Channel s r
  -> ActionM (Either PayloadError Jwt.Jwt)
getJwePayloadFromBody channel = do
  claims <- body
  let obj = Aeson.eitherDecode claims :: Either String Aeson.Object
  liftIO $ case obj of
    Right input  -> encodePayload channel Outgoing input
    Left errText -> return $ Left $ AesonParseError errText
