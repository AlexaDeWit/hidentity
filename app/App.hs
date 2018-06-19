{-# LANGUAGE OverloadedStrings #-}

module App
 ( app
 ) where

import Conf
import Entity      (Speedledger(..), Nordea(..))
import Web.Scotty
import Protolude   (($), IO, show, (>>=), Maybe(..), liftA2, putStrLn, return, (<&>))
import Data.Aeson  (decode)
import Jose.Jwk    (Jwk(..))
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Monoid (mconcat)
import Data.Text.Lazy (unpack, Text, pack)

app :: Environment -> IO ()
app env = do
  conf <- C.load [C.Required $ unpack $ confFileName env]
  maybeKeyRing <- makeKeyRing conf
  case maybeKeyRing of
    Nothing      -> putStrLn $ pack "Keyring could not be loaded"
    Just keyRing -> do
       scotty 3000 $
        get "/:word" $ do
          beam <- param "word"
          html $ mconcat ["<h1>", show env, ", ", beam, " me up!</h1>"]


makeKeyRing :: C.Config -> IO (Maybe KeyRing)
makeKeyRing conf = do
  slKeyText     <- C.lookup conf "IdentityKeys.Speedledger"
  nordeaKeyText <- C.lookup conf "IdentityKeys.Ndc"
  let slKey = slKeyText >>= decode <&> Speedledger
  let ndcKey = nordeaKeyText >>= decode <&> Nordea
  return $ liftA2 KeyRing slKey ndcKey