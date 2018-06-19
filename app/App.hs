{-# LANGUAGE OverloadedStrings #-}

module App
 ( app
 ) where

import Conf
import Entity      (Speedledger(..), Nordea(..))
import Data.String (String)
import Data.ByteString.Lazy (ByteString)
import Web.Scotty
import Protolude   (($), IO, show, (>>=), Maybe(..), liftA2, putStrLn, return, (<&>), (.), Either(..))
import Data.Either.Combinators (maybeToRight, mapRight)
import Data.Aeson  (decode, eitherDecode)
import Jose.Jwk    (Jwk(..))
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Monoid (mconcat)
import Data.Text.Lazy (unpack, Text, pack, append)

app :: Environment -> IO ()
app env = do
  conf <- C.load [C.Required $ unpack $ confFileName env]
  maybeKeyRing <- makeKeyRing conf
  case maybeKeyRing of
    Left errText      -> putStrLn $ append (pack "Keyring could not be loaded, reason: ") (pack errText)
    Right keyRing -> do
       scotty 3000 $
        get "/:word" $ do
          beam <- param "word"
          html $ mconcat ["<h1>", show env, ", ", beam, " me up!</h1>"]


makeKeyRing :: C.Config -> IO (Either String KeyRing)
makeKeyRing conf = do
  slKeyText     <- C.lookup conf "IdentityKeys.Speedledger"
  nordeaKeyText <- C.lookup conf "IdentityKeys.Ndc"
  let slKeyStr = maybeToRight "Could not load Speedledger key" slKeyText :: Either String ByteString
  let ndcKeyStr = maybeToRight "Could not load Nordea key" nordeaKeyText :: Either String ByteString
  let slKey = slKeyStr >>= eitherDecode <&> Speedledger
  let ndcKey = ndcKeyStr >>= eitherDecode <&> Nordea
  return $ liftA2 KeyRing slKey ndcKey
