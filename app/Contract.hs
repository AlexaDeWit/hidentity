module Contract
  ( Contract(..)
  , Channel(..)
  , PayloadFormat(..)
  , Keyable(..)
  , Direction(..)
  , symmetricalChannel
  ) where

import Protolude               (($), Either(..), IO, panic, return)
import Data.ByteString         (ByteString)
import Jose.Jwk                (Jwk)
import Data.Text               (pack)
import Jose.Jwa                (JweAlg, Enc, JwsAlg)
import Jose.Jwe                (jwkDecode)
import Jose.Jwt                (JwtContent(..), JwtError(..), JwtClaims(..))

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

data PayloadError
  = JwtErr JwtError
  | IncorrectFormatError

symmetricalChannel :: (Keyable sender, Keyable recipient) => sender -> recipient -> PayloadFormat -> Channel sender recipient
symmetricalChannel s r form = Channel sendingContract receivingContract where
    sendingContract   = Contract form s r
    receivingContract = Contract form r s

retrievePayload :: (Keyable s, Keyable r) => Channel s r -> Direction -> ByteString -> IO (Either PayloadError JwtClaims)
retrievePayload channel direction =
  let
    unpack :: (Keyable s, Keyable r) => Contract s r -> ByteString -> IO (Either PayloadError JwtClaims)
    unpack contract input = case payloadFormat contract of
      WrappedJwt jwsAlg jweAlg enc -> do
        let recipientKey = retrieveKey $ recipient contract
        result <- jwkDecode recipientKey input
        case result of
          Right (Unsecured content) -> decodeAndUnpackJwt (retrieveKey $ sender contract) jwsAlg content
          Right _                   -> return $ Left IncorrectFormatError
          Left err                  -> return $ Left $ JwtErr err
  in
  case direction of
    Incoming -> unpack $ incoming channel
    Outgoing -> unpack $ outgoing channel

decodeAndUnpackJwt :: Jwk -> JwsAlg -> ByteString -> IO (Either PayloadError JwtClaims)
decodeAndUnpackJwt = panic $ pack $ "Typecheck"