module Contract
  ( Contract(..)
  , Channel(..)
  , PayloadFormat(..)
  , Keyable(..)
  , Direction(..)
  , symmetricalChannel
  ) where

import Protolude               (Maybe, panic, ($))
import Data.ByteString.Lazy    (ByteString)
import Jose.Jwk                (Jwk)
import Data.Text               (pack)
import Jose.Jwa                (JweAlg, Enc, JwsAlg)


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
  | PlainJwt JwsAlg


data Direction
  = Incoming
  | Outgoing


symmetricalChannel :: (Keyable sender, Keyable recipient) => sender -> recipient -> PayloadFormat -> Channel sender recipient
symmetricalChannel s r form = Channel sendingContract receivingContract where
    sendingContract   = Contract form s r
    receivingContract = Contract form r s

retrievePayload :: (Keyable s, Keyable r) => Channel s r -> Direction -> ByteString -> Maybe ByteString
retrievePayload channel Incoming = applyContract $ incoming channel
retrievePayload channel Outgoing = applyContract $ outgoing channel

applyContract :: (Keyable s, Keyable r) => Contract s r -> ByteString -> Maybe ByteString
applyContract = panic $ pack "Thus I die. Thus, thus, thus. Now I am dead"
