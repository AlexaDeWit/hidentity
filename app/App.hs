{-# LANGUAGE OverloadedStrings #-}

module App
 ( app
 ) where


import Protolude               (($), IO, show, (>>=), liftA2, putStrLn, return, (<&>), Either(..))
import Data.String             (String)
import Data.ByteString.Lazy    (ByteString)
import Web.Scotty
import Data.Either.Combinators (maybeToRight)
import Data.Aeson              (eitherDecode)
import Data.Monoid             (mconcat)
import Data.Text.Lazy          (unpack, pack, append)

import Conf
import Entity                  (Speedledger(..), Nordea(..))

import qualified Data.Configurator       as C
import qualified Data.Configurator.Types as C

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
  let slKeyEither = slKeyStr >>= eitherDecode <&> Speedledger
  let ndcKeyEither = ndcKeyStr >>= eitherDecode <&> Nordea
  return $ liftA2 KeyRing slKeyEither ndcKeyEither
