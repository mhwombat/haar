module Codec.GrayQC
  (
    test
  ) where

import Codec.Gray
import Data.List (nub)
import Data.Word (Word8)
import Test.Framework as TF (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>), Property, Gen, Arbitrary, arbitrary, choose, 
  property, sized)

prop_successive_values_differ_in_one_place1 :: Int -> Property
prop_successive_values_differ_in_one_place1 k = k > 0 ==>
  nub (diffCounts xs) == [1]
    where xs = grayCodes k'
          k' = min 10 k -- avoid long tests

data TestParms = TestParms String Int deriving Show

sizedTestParms :: Int -> Gen TestParms
sizedTestParms n = do
  let n' = 2 + (min 8 n) -- keep number of digits and bits low to speed up tests
  let digits = take n' ['a' .. 'z']
  k <- choose (1,min 4 (n+1))
  return $ TestParms digits k

instance Arbitrary TestParms where
  arbitrary = sized sizedTestParms
  
prop_successive_values_differ_in_one_place2 :: TestParms -> Property
prop_successive_values_differ_in_one_place2 (TestParms ds k) = property $
  nub (diffCounts xs) == [1]
    where xs = naryGrayCodes ds k

diffCounts :: Eq b => [[b]] -> [Int]
diffCounts [] = []
diffCounts [_] = []
diffCounts (x1:x2:xs) = (diffCount x1 x2) : diffCounts (x2:xs)

diffCount :: Eq b => [b] -> [b] -> Int
diffCount as bs = length $ filter (\x -> x) $ zipWith (/=) as bs

prop_encoding_round_trippable :: Int -> Property
prop_encoding_round_trippable n =
  n >= 0 ==> (grayToIntegral . integralToGray $ n) == n

prop_decoding_round_trippable :: Int -> Property
prop_decoding_round_trippable n =
  n >= 0 ==> (integralToGray . grayToIntegral $ n) == n

prop_integralToGray_same_as_encode :: Word8 -> Property
prop_integralToGray_same_as_encode n =
  property $ integralToGray n == boolsToIntegral bits
    where bits = (grayCodes 8) !! (fromIntegral n)

boolsToIntegral :: Num c => [Bool] -> c
boolsToIntegral bs = f 0 1 . reverse $ bs
  where f total _ [] = total
        f total factor (b:bs') = f total' (factor*2) bs'
          where total' = if b then total + factor else total
                
-- integralToBools :: (Integral a, Bits a) => a -> [Bool]
-- integralToBools 0 = []
-- integralToBools n = integralToBools (n `shiftR` 1) ++ [f (n `mod` 2)]
--   where f 0 = False
--         f 1 = True

test :: Test
test = testGroup "Codec.GrayQC"
  [
    testProperty "prop_successive_values_differ_in_one_place1"
      prop_successive_values_differ_in_one_place1,
    testProperty "prop_successive_values_differ_in_one_place2"
      prop_successive_values_differ_in_one_place2,
    testProperty "prop_encoding_round_trippable"
      prop_encoding_round_trippable,
    testProperty "prop_decoding_round_trippable"
      prop_decoding_round_trippable,
    testProperty "prop_integralToGray_same_as_encode"
      prop_integralToGray_same_as_encode
  ]


