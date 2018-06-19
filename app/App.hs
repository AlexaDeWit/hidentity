{-# LANGUAGE OverloadedStrings #-}

module App
 ( app
 ) where

import Conf
import Web.Scotty
import Protolude   (($), IO, show, (>>=), Maybe, liftA2, putStrLn, return)
import Data.Aeson  (decode)
import Jose.Jwk    (Jwk(..))
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Monoid (mconcat)
import Data.Text.Lazy (unpack, Text)

app :: Environment -> IO ()
app env = scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>", show env, ", ", beam, " me up!</h1>"]



loadConfiguration :: Environment -> IO ()
loadConfiguration env = do
  conf <- C.load [C.Required $ unpack $ confFileName env]
  let text = "Conf Loaded" :: Text
  putStrLn text

keyConfig :: C.Config -> IO (Maybe (Jwk, Jwk))
keyConfig conf = do
  slKeyText     <- C.lookup conf "IdentityKeys.Speedledger"
  nordeaKeyText <- C.lookup conf "IdentityKeys.Ndc"
  let slKey = slKeyText >>= decode
  let ndcKey = nordeaKeyText >>= decode
  return $ liftA2 (,) slKey ndcKey