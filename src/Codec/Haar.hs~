------------------------------------------------------------------------
-- |
-- Module      :  Codec.Gray
-- Copyright   :  (c) Amy de Buitléir 2011-2012
-- License     :  BSD-style
-- Maintainer  :  amy@nualeargais.ie
-- Stability   :  experimental
-- Portability :  portable
--
-- Gray encoding schemes. A Gray code is a list of values such that two
-- successive values differ in only one digit. Usually the term /Gray
-- code/ refers to the Binary Reflected Gray code (BRGC), but non-binary
-- Gray codes have also been discovered. Some Gray codes are also
-- /cyclic/: the last and first values differ in only one digit.
--
------------------------------------------------------------------------
module Codec.Gray 
  (
    grayCodes,
    integralToGray,
    grayToIntegral,
    naryGrayCodes
  ) where

import Data.List (foldl')
import Data.Bits (Bits, shiftR, xor)

-- | @'grayCodes' k@ generates the list of Binary Reflected Gray Code
--   (BRGC) numbers of length k. This code is cyclic.
grayCodes :: Int -> [[Bool]]
grayCodes 0 = [[]]
grayCodes k = 
  let xs = grayCodes (k-1) in map (False:) xs ++ map (True:) (reverse xs)

-- | @'integralToGray' n@ encodes @n@ using a BRGC, and returns the
--   resulting bits as an integer. For example, encoding @17@ in BRGC
--   results in @11001@, or 25. So @integralToGray 17@ returns @25@.
integralToGray :: Bits a => a -> a
integralToGray n = (n `shiftR` 1) `xor` n

-- | @'grayToIntegral' n@ decodes @n@ using a BRGC, and returns the
--   resulting integer. For example, 25 is @11001@, which is the code
--   for 17. So @grayToIntegral 25@ returns @17@.
grayToIntegral :: (Num a, Bits a) => a -> a
grayToIntegral n = f n (n `shiftR` 1)
  where f k m | m /= 0     = f (k `xor` m) (m `shiftR` 1)
              | otherwise = k
  
-- | @'naryGrayCodes' xs k@ generates a non-Boolean (or n-ary) Gray code
--   of length @k@ using the elements of @xs@ as \"digits\". This code
--   is cyclic.
--
--   Ex: @'naryGrayCodes' \"012\" 4@ generates a ternary Gray code that
--   is four digits long.
naryGrayCodes :: [a] -> Int -> [[a]]
naryGrayCodes xs 1 = map (\x -> [x]) xs
naryGrayCodes xs k = snd $ foldl' prefixAndShift (ys,[]) xs'
  where ys = naryGrayCodes xs 1
        xs' = naryGrayCodes xs (k-1)

-- | Shift elements right.
shift :: [a] -> [a]
shift as = last as : init as

prefixAndShift :: ([[a]],[[a]]) -> [a] -> ([[a]],[[a]])
prefixAndShift (ys,zs) xs = (shift ys, zs ++ (map (xs++) ys))

