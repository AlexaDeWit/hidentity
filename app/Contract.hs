module Contract
  ( Contract(..)
  , Channel(..)
  , PayloadFormat(..)
  , Keyable(..)
  , Direction(..)
  , Claims
  , PayloadError(..)
  , symmetricalChannel
  , retrievePayload
  , encodePayload
  ) where

import Protolude               (($), Either(..), IO, return, Maybe(..), Show, show)
import Data.ByteString         (ByteString)
import Data.String             (String)
import Data.ByteString.Lazy    (fromStrict, toStrict)
import Data.Either.Combinators (mapLeft)
import Jose.Jwk                (Jwk)
import Jose.Jwa                (JweAlg, Enc, JwsAlg)
import Jose.Jwe                (jwkDecode, jwkEncode)
import Jose.Jwt                (JwtContent(..), JwtError(..), JwtEncoding(..))

import qualified Data.Aeson    as Aeson
import qualified Jose.Jwt      as Jwt

class Keyable a where
  retrieveKey :: a -> Jwk


data (Keyable s, Keyable r) => Contract s r
  = Contract
  { payloadFormat :: PayloadFormat
  , sender        :: s
  , recipient     :: r
  }

data (Keyable e1, Keyable e2) => Channel e1 e2
  = Channel
  { outgoing :: Contract e1 e2
  , incoming :: Contract e2 e1
  }

data PayloadFormat
  = WrappedJwt JwsAlg JweAlg Enc


data Direction
  = Incoming
  | Outgoing

data FormatError
  = NoNestedJweContent String
  | InextricableJwtClaims String
  deriving (Show)

data PayloadError
  = JwtErr JwtError
  | FormatError FormatError
  | AesonParseError String
  deriving (Show)

type Claims = Aeson.Object

symmetricalChannel :: (Keyable sender, Keyable recipient) => sender -> recipient -> PayloadFormat -> Channel sender recipient
symmetricalChannel s r form = Channel sendingContract receivingContract where
    sendingContract   = Contract form s r
    receivingContract = Contract form r s

retrievePayload
  :: (Keyable s, Keyable r)
  => Channel s r
  -> Direction
  -> ByteString
  -> IO (Either PayloadError Claims)
retrievePayload channel direction =
  let
    unpack :: (Keyable s, Keyable r) => Contract s r -> ByteString -> IO (Either PayloadError Claims)
    unpack contract input = case payloadFormat contract of
      WrappedJwt jwsAlg _ _ -> do
        let recipientKey = retrieveKey $ recipient contract
        result <- jwkDecode recipientKey input
        case result of
          Right (Jwe (_, content))  -> decodeAndUnpackJwt (retrieveKey $ sender contract) jwsAlg content
          Right inner               -> return $ Left $ FormatError $ NoNestedJweContent $ show inner
          Left err                  -> return $ Left $ JwtErr err
  in
  case direction of
    Incoming -> unpack $ incoming channel
    Outgoing -> unpack $ outgoing channel

decodeAndUnpackJwt :: Jwk -> JwsAlg -> ByteString -> IO (Either PayloadError Claims)
decodeAndUnpackJwt key alg input = do
  result <- Jwt.decode [key] (Just $ JwsEncoding alg) input
  return $ case result of
    Right (Jws (_, claimsBlob)) -> mapLeft AesonParseError $ Aeson.eitherDecode $ fromStrict claimsBlob
    Right inner                 -> Left $ FormatError $ InextricableJwtClaims $ show inner
    Left err                    -> Left $ JwtErr err

encodePayload
  :: (Keyable s, Keyable r, Aeson.ToJSON payload)
  => Channel s r
  -> Direction
  -> payload
  -> IO (Either PayloadError Jwt.Jwt)
encodePayload channel direction =
  let
    pack
      :: (Keyable s, Keyable r, Aeson.ToJSON payload)
      => Contract s r
      -> payload
      -> IO (Either PayloadError Jwt.Jwt)
    pack contract inputObj = case payloadFormat contract of
      WrappedJwt jwsAlg jweAlg enc -> do
        let recipientKey = retrieveKey $ recipient contract
        let senderKey = retrieveKey $ sender contract
        encodedJwt <- encodeAndPackJwt senderKey jwsAlg inputObj
        case encodedJwt of
          Right nestedJwt -> do
            jwePayload <- jwkEncode jweAlg enc recipientKey (Jwt.Nested nestedJwt)
            return $ mapLeft JwtErr jwePayload
          err             -> return err
  in
  case direction of
    Incoming -> pack $ incoming channel
    Outgoing -> pack $ outgoing channel

encodeAndPackJwt :: Aeson.ToJSON payload => Jwk -> JwsAlg -> payload -> IO (Either PayloadError Jwt.Jwt)
encodeAndPackJwt key alg inputObj = do
  let input = Aeson.encode inputObj
  packed <- Jwt.encode [key] (JwsEncoding alg) (Jwt.Claims $ toStrict input)
  return $ mapLeft JwtErr packed
