module Main where

import Protolude (IO)
import App       (app)
import Conf      (ApplicationConfiguration(..))

main :: IO ()
main = app Production