{-# LANGUAGE OverloadedStrings #-}

module App
 ( app
 ) where

import Conf
import Web.Scotty
import Protolude   (($), IO)
import Data.Monoid (mconcat)

app :: ApplicationConfiguration -> IO ()
app _ = scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]