------------------------------------------------------------------------
-- |
-- Module      :  Codec.HaarQC
-- Copyright   :  (c) Amy de Buitléir 2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Test suite.
--
------------------------------------------------------------------------
module Codec.HaarQC
  (
    test
  ) where

import Debug.Trace
import Control.Monad (when)
import Codec.Haar
import Test.Framework as TF (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Gen, Arbitrary, Property, arbitrary, sized,
  property, vectorOf, choose)

nearestPowerOf2 :: Int -> Int
nearestPowerOf2 n = 2^k
  where n' = fromIntegral n :: Float
        k = floor (logBase 2 n') :: Int

equiv0D :: Double -> Double -> Bool
equiv0D x y = abs (x - y) < 1e-8

equiv1D :: [Double] -> [Double] -> Bool
equiv1D xs ys = and $ zipWith equiv0D xs ys

equiv2D :: [[Double]] -> [[Double]] -> Bool
equiv2D xss yss = and $ zipWith equiv1D xss yss

newtype Test1D = Test1D [Double] deriving (Eq, Show)

sizedArray :: Arbitrary a => Int -> Gen [a]
sizedArray n = vectorOf (nearestPowerOf2 n) arbitrary

instance Arbitrary Test1D where
  arbitrary = fmap Test1D $ sized sizedArray

newtype Test2D = Test2D [[Double]] deriving (Eq, Show)

sizedTest2D :: Int -> Gen Test2D
sizedTest2D n = do
  r <- fmap nearestPowerOf2 $ choose (0,n)
  when (trace (show r) r /= nearestPowerOf2 r) $ error "r problem"
  let c = nearestPowerOf2 (n - r)
  when (trace (show c) c /= nearestPowerOf2 c) $ error "c problem"
  fmap Test2D $ vectorOf r (vectorOf c arbitrary)

instance Arbitrary Test2D where
  arbitrary = sized sizedTest2D

prop_roundtrip1D :: Test1D -> Property
prop_roundtrip1D (Test1D xs) =
  property $ (unHaar1D . haar1D $ xs) `equiv1D` xs

prop_roundtrip2D :: Test2D -> Property
prop_roundtrip2D (Test2D xs) =
  property $ (unHaar2D . haar2D $ xs) `equiv2D` xs

test :: Test
test = testGroup "Codec.HaarQC"
  [
    testProperty "prop_roundtrip1D" prop_roundtrip1D,
    testProperty "prop_roundtrip2D" prop_roundtrip2D
  ]


