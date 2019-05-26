{-# LANGUAGE ViewPatterns #-}

module Lib
    ( someFunc
    , entropy
    ) where
import Data.Maybe (fromMaybe)
import Data.List (foldl1')

someFunc :: IO ()
someFunc = putStrLn $ show $ entropy [7, 4, 4] 15 2

entropy :: [Int] -> Int -> Int -> Double
entropy typeFrequencies totalElements logarithmicBase =
  -(foldl1' (+) $ map (\p -> p * (logBase b p)) probabilities)
    where
      is = map fromIntegral typeFrequencies
      l  = fromIntegral totalElements
      b  = fromIntegral logarithmicBase
      probabilities = map (flip (/) $ l) $ is
