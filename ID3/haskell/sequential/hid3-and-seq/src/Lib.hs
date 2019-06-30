{-# LANGUAGE DataKinds, FlexibleContexts, QuasiQuotes, TemplateHaskell, OverloadedStrings #-}

module Lib
    ( someFunc
    , entropy'
    , entropy
    -- , loadRows
    -- , streamRows
    -- , ratio
    -- , minIncome
    -- , minMaxIncome
    , streamSpamOrHam
    , loadSpamOrHam
    , selectSuspiciousWords
    , onlySuspiciousWords
    , SpamOrHam
    , SpamId
    , SuspiciousWords
    , UnknownSender
    , Images
    , SpamClass
    , spamId
    , suspiciousWords
    , unknownSender
    , images
    , spamClass
    ) where
import Data.Foldable (foldl')
import Data.Foldable as F
import qualified Control.Foldl as L
import Data.Vinyl (rcast)
import Frames
import Frames.CSV (readTableOpt, rowGen, RowGen(..))
import Pipes hiding (Proxy)
import Lens.Micro.Extras
import qualified Pipes.Prelude as P

-- Data set from http://vincentarelbundock.github.io/Rdatasets/datasets.html
tableTypes "SpamOrHam" "data/SpamAnalysis.csv"

-- someFunc :: IO ()
-- someFunc = putStrLn $ show $ entropy' 15 [7, 4, 4] 2

someFunc :: IO ()
someFunc = show <$> totalSpamOrHamEntropy >>= putStrLn

streamSpamOrHam :: MonadSafe m => Producer SpamOrHam m ()
streamSpamOrHam = readTableOpt spamOrHamParser "data/SpamAnalysis.csv"

loadSpamOrHam :: IO (Frame SpamOrHam)
loadSpamOrHam = inCoreAoS streamSpamOrHam

-- minIncome :: IO (Maybe Int)
-- minIncome = (\rows -> L.fold L.minimum (view income <$> rows)) <$> loadRows

-- minIncome' :: IO (Maybe Int)
-- minIncome' = (\rows -> L.fold (L.handles income L.minimum) rows) <$> loadRows

selectSuspiciousWords :: IO (Frame Bool)
selectSuspiciousWords = (\rows -> view suspiciousWords <$> rows) <$> loadSpamOrHam

onlySuspiciousWords :: SpamOrHam -> Record '[SuspiciousWords]
onlySuspiciousWords = rcast

totalSpamOrHamEntropy :: IO (Double)
totalSpamOrHamEntropy = do
  soh <- loadSpamOrHam
  spamClassCol <- (\soh' -> F.toList $ view spamClass <$> soh') <$> loadSpamOrHam
  return $ entropy (frameLength soh) [ length $ filter (== "spam") spamClassCol
                                     , length $ filter (== "ham") spamClassCol
                                     ]

-- minMaxIncome :: IO (Maybe Int, Maybe Int)
-- minMaxIncome = (\rows -> L.fold (L.handles income minMax) rows) <$> loadRows
--   where minMax = (,) <$> L.minimum <*> L.maximum

-- ratio :: Record '[Income, Prestige] -> Double
-- ratio = runcurry' (\i p -> fromIntegral i / p)

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
