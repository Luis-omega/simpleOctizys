module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Inference as Inference


main :: IO ()
main =
  defaultMain $
    testGroup
      "unit tests"
      [ Inference.tests
      ]
