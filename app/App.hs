{-# LANGUAGE OverloadedStrings #-}

module App
 ( app
 ) where

import Conf
import Web.Scotty
import Protolude   (($), IO, show)
import Data.Monoid (mconcat)

app :: Environment -> IO ()
app env = scotty 3000 $
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>", show env, ", ", beam, " me up!</h1>"]