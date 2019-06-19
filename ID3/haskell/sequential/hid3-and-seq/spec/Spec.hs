{-# LANGUAGE ViewPatterns #-}

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib
import Data.List (foldl')
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

  describe "Lib.entropy'" $ do
    it "returns a non-negative number" $
      (property :: (([Positive Int] -> NonNegative Int -> Bool) -> Property)) $
        \(coerce -> xs) (coerce -> b) ->
          let sumXs = foldl' (+) 0 xs
          in case b of
            1 -> case xs of
              [x] -> isNaN $ entropy' x xs b
              []  -> entropy' sumXs xs b == 0.0
              _   -> isInfinite $ entropy' sumXs xs b
            _ -> entropy' sumXs xs b >= 0

    it "returns the weighted sum of the logs of the probabilities" $
      (property :: ((NonEmptyList (Positive Int) -> NonNegative Int -> Bool) -> Property)) $
        \(coerce -> xs) (coerce -> b) ->
          let sumXs = foldl' (+) 0 xs
              probabilities =
                map (\x -> fromIntegral x / (fromIntegral sumXs)) xs
              weightSumOfProbLogs =
                map (\p -> (*) p $ logBase (fromIntegral b) p) probabilities
          in case b of
            1 -> case xs of
              [x] -> isNaN $ entropy' x xs b
              []  -> entropy' sumXs xs b == 0.0
              _   -> isInfinite $ entropy' sumXs xs b
            _ -> entropy' sumXs xs b == -foldl' (+) 0.0 weightSumOfProbLogs

    it "returns a higher value for higher-entropy' lists" $
      entropy' 20 [5, 5, 5, 5] 2 > (entropy' 20 [1, 2, 2, 10, 5] 2) `shouldBe` True

  describe "Lib.entropy" $ do
    it "entropy xsum xs returns the same thing as entropy' xsum xs 2" $
      (property :: (NonEmptyList (Positive Int) -> Bool) -> Property) $
        \(coerce -> xs) ->
          let xsum = foldl' (+) 0 (xs :: [Int])
          in entropy xsum xs == entropy' xsum xs 2

    it "returns ~5.700 bits for the entropy of drawing a specific card from a 52 card deck" $
      (entropy 52 $ take 52 $ repeat 1) `shouldBe` 5.700439718141095

    it "returns 2.0 bits for the entropy of drawing a specific suit from a 52 card deck" $
      (entropy 52 $ take 4 $ repeat 13) `shouldBe` 2.0

