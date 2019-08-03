{-# LANGUAGE ViewPatterns, TemplateHaskell, FlexibleContexts, DataKinds #-}

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib
import Data.Text
import qualified Data.List as L
import Data.Coerce (coerce)

import Frames.CSV (readTableOpt)
import Frames (tableTypes, Frame, inCoreAoS)
import Pipes (Producer)

tableTypes "PlayingCards" "data/PlayingCards.csv"

loadPlayingCards :: IO (Frame PlayingCards)
loadPlayingCards = inCoreAoS $ readTableOpt playingCardsParser "data/PlayingCards.csv"


tableTypes "EcoVeg" "data/EcologicalVegetation.csv"

loadEcoVeg :: IO (Frame EcoVeg)
loadEcoVeg = inCoreAoS $ readTableOpt ecoVegParser "data/EcologicalVegetation.csv"


gt1Int :: Gen Int
gt1Int = sized $ \n -> choose (2, n)

main :: IO ()
main = hspec $ do
  describe "Lib.entropy'" $ do
    it "returns a non-negative number" $
      (property :: (([Positive Int] -> NonNegative Int -> Bool) -> Property)) $
        \(coerce -> xs) (coerce -> b) ->
          let sumXs = L.foldl' (+) 0 xs
          in case b of
            1 -> case xs of
              [x] -> isNaN $ entropy' x xs b
              []  -> entropy' sumXs xs b == 0.0
              _   -> isInfinite $ entropy' sumXs xs b
            _ -> entropy' sumXs xs b >= 0

    it "returns the weighted sum of the logs of the probabilities" $
      (property :: ((NonEmptyList (Positive Int) -> NonNegative Int -> Bool) -> Property)) $
        \(coerce -> xs) (coerce -> b) ->
          let sumXs = L.foldl' (+) 0 xs
              probabilities =
                Prelude.map (\x -> fromIntegral x / (fromIntegral sumXs)) xs
              weightSumOfProbLogs =
                Prelude.map (\p -> (*) p $ logBase (fromIntegral b) p) probabilities
          in case b of
            1 -> case xs of
              [x] -> isNaN $ entropy' x xs b
              []  -> entropy' sumXs xs b == 0.0
              _   -> isInfinite $ entropy' sumXs xs b
            _ -> entropy' sumXs xs b == -L.foldl' (+) 0.0 weightSumOfProbLogs

    it "returns a higher value for higher-entropy' lists" $
      entropy' 20 [5, 5, 5, 5] 2 > (entropy' 20 [1, 2, 2, 10, 5] 2) `shouldBe` True

  describe "Lib.entropy" $ do
    it "entropy xsum xs returns the same thing as entropy' xsum xs 2" $
      (property :: (NonEmptyList (Positive Int) -> Bool) -> Property) $
        \(coerce -> xs) ->
          let xsum = L.foldl' (+) 0 (xs :: [Int])
          in entropy xsum xs == entropy' xsum xs 2

    it "returns ~5.700 bits for the entropy of drawing a specific card from a 52 card deck" $
      (entropy 52 $ Prelude.take 52 $ repeat 1) `shouldBe` 5.700439718141095

    it "returns 2.0 bits for the entropy of drawing a specific suit from a 52 card deck" $
      (entropy 52 $ Prelude.take 4 $ repeat 13) `shouldBe` 2.0


  describe "Lib.totalSetEntropy" $ do
    it "Returns 1.0 bits for the Spam dataset entropy" $ do
      spamFrame <- loadSpamOrHam
      (totalSetEntropy spamClass spamFrame) `shouldBe` 1.0

    it "Returns 1.5566567074628228 bits for the Ecological Vegetation dataset entropy" $ do
      ecoVegFrame <- loadEcoVeg
      (totalSetEntropy vegetation ecoVegFrame) `shouldBe` 1.5566567074628228

    it "Returns 2.0 bits for the Playing Cards dataset entropy when Suit is the target feature" $ do
      cardsFrame <- loadPlayingCards
      (totalSetEntropy suit cardsFrame) `shouldBe` 2.0

