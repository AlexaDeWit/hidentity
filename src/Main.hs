{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Protolude   (($), IO)
import Data.Monoid (mconcat)

main :: IO ()
main = scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]