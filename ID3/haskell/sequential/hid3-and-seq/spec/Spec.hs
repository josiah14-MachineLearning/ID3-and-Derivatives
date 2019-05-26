{-# LANGUAGE ViewPatterns #-}

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib
import Data.List (foldl1')
import Data.Coerce (coerce)

gt1Int :: Gen Int
gt1Int = sized $ \n -> choose (2, n)

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException

  describe "Lib.entropy" $ do
    it "returns a non-zero number" $
      (property :: ((NonEmptyList (Positive Int) -> NonNegative Int -> Bool) -> Property)) $
        \(coerce -> xs) (coerce -> b) ->
          let sumXs = foldl1' (+) xs
          in case b of
            1 -> case xs of
              [x] -> isNaN $ entropy xs x b
              _   -> isInfinite $ entropy xs sumXs b
            _ -> entropy xs sumXs b >= 0

    it "returns the weighted sum of the logs of the probabilities" $
      (property :: ((NonEmptyList (Positive Int) -> NonNegative Int -> Bool) -> Property)) $
        \(coerce -> xs) (coerce -> b) ->
          let sumXs = foldl1' (+) xs
              probabilities =
                map (\x -> fromIntegral x / (fromIntegral sumXs)) xs
              weightSumOfProbLogs =
                map (\p -> (*) p $ logBase (fromIntegral b) p) probabilities
          in case b of
            1 -> case xs of
              [x] -> isNaN $ entropy xs x b
              _   -> isInfinite $ entropy xs sumXs b
            _ -> entropy xs sumXs b == -foldl1' (+) weightSumOfProbLogs

    it "returns a higher value for higher-entropy lists" $
      entropy [5, 5, 5, 5] 20 2 > (entropy [1, 2, 2, 10, 5] 20 2) `shouldBe` True
