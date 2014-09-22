module Main where

import Codec.HaarQC ( test )

import Test.Framework as TF ( defaultMain, Test )

tests :: [TF.Test]
tests = 
  [ 
    Codec.HaarQC.test
  ]

main :: IO ()
main = defaultMain tests
