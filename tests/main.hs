module Main where

import Test.Framework (defaultMain, testGroup)

import qualified Tests.Parser

main :: IO ()
main = defaultMain
  [ testGroup "Tests.Parser.tests" Tests.Parser.tests
  ]
