{-# LANGUAGE ViewPatterns #-}

module Lib
    ( someFunc
    , entropy
    ) where
import Data.Maybe (fromMaybe)
import Data.Foldable (foldl')

someFunc :: IO ()
someFunc = putStrLn $ show $ entropy 15 2 [7, 4, 4]

entropy :: (Foldable f, Floating b) => Int -> Int -> f Int -> b
entropy totalElements logarithmicBase itemFrequencies =
  negate $ foldl' (\entropy f -> entropy + itemEntropy f) 0 itemFrequencies
    where
      itemEntropy ifreq = let prob = (fromIntegral ifreq) / l
                          in prob * logBase b prob
      l  = fromIntegral totalElements
      b  = fromIntegral logarithmicBase
