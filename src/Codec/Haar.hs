------------------------------------------------------------------------
-- |
-- Module      :  Codec.Haar
-- Copyright   :  (c) Amy de Buitléir 2014
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Haar Wavelet transforms.
--
------------------------------------------------------------------------
module Codec.Haar 
  (
    haar1D,
    unHaar1D,
    haar2D,
    unHaar2D
  ) where

import Data.List (transpose)
import Data.List.Split (chunksOf)

haar :: (Num a, Fractional a) => a -> a -> (a, a)
haar x y = (xMean, x - xMean)
  where xMean = (x + y)/2

-- | Perform a Haar wavelet transform on a one-dimensional array.
--   The length of the array must be a power of 2, otherwise an error
--   will occur.
haar1D :: (Num a, Fractional a) => [a] -> [a]
haar1D [] = []
haar1D xs = haar1D' (xs,[])

haar1D' :: (Num a, Fractional a) => ([a],[a]) -> [a]
haar1D' ([x], cs) = x:cs
haar1D' (xs, cs) = haar1D' (xs', cs' ++ cs)
  where (xs', cs') = unzip . map f . chunksOf 2 $ xs
        f (x:y:[]) = haar x y
        f _ = error "logic error"

-- | Undo a Haar wavelet transform, recovering the original
--   one-dimensional array.
unHaar1D :: (Num a, Fractional a) => [a] -> [a]
unHaar1D (x:xs) = unHaar1D' [x] xs
unHaar1D [] = []

unHaar1D' :: (Num a, Fractional a) => [a] -> [a] -> [a]
unHaar1D' xs [] = xs
unHaar1D' xs cs = unHaar1D' xs' cs'
  where xs' = concat $ zipWith f xs cs
        cs' = leftover xs cs
        f x c = [x+c, x-c]

leftover :: [a] -> [b] -> [b]
leftover [] bs = bs
leftover _ [] = []
leftover (_:as) (_:bs) = leftover as bs

-- | Perform a Haar wavelet transform on a two-dimensional array.
--   The number of rows and columns must both be powers of 2,
--   otherwise an error will occur.
haar2D :: (Num a, Fractional a) => [[a]] -> [[a]]
haar2D = transpose . map haar1D . transpose . map haar1D

-- | Undo a Haar wavelet transform, recovering the original
--   two-dimensional array.
unHaar2D :: (Num a, Fractional a) => [[a]] -> [[a]]
unHaar2D = map unHaar1D . transpose . map unHaar1D . transpose
