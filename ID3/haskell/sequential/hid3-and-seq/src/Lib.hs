module Lib
    ( someFunc
    , entropy'
    , entropy
    ) where
import Data.Foldable (foldl')

someFunc :: IO ()
someFunc = putStrLn $ show $ entropy' 15 [7, 4, 4] 2

entropy' :: (Foldable f, Floating b) => Int -> f Int -> Int -> b
entropy' totalElements itemFrequencies logarithmicBase =
  negate $ foldl' (\entropyAcc f -> entropyAcc + itemEntropy f) 0 itemFrequencies
    where
      itemEntropy ifreq = let prob = (fromIntegral ifreq) / l
                          in prob * logBase b prob
      l  = fromIntegral totalElements
      b  = fromIntegral logarithmicBase

entropy :: (Foldable f, Floating b) => Int -> f Int -> b
entropy totalElements itemFrequencies = entropy' totalElements itemFrequencies 2
