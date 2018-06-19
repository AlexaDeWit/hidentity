module Main where

import Protolude
import Test.Hspec

main :: IO ()
main = hspec $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (Just 23 :: Maybe Int)