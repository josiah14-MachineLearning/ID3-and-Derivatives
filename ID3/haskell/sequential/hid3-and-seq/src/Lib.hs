{-# LANGUAGE TypeOperators,
             DataKinds,
             FlexibleContexts,
             QuasiQuotes,
             TemplateHaskell,
             OverloadedStrings,
             AllowAmbiguousTypes,
             RankNTypes, KindSignatures #-}

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
    , selectSuspiciousWords
    , selectUnknownSender
    , onlySuspiciousWords
    , uniqueSpam
    , totalSetEntropy
    , loadSpamOrHam
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
import Data.List.Unique
import qualified Control.Foldl as L
import Data.Vinyl (rcast)
import Data.Vinyl.Lens (RElem)
import Data.Vinyl.TypeLevel (RIndex)
import Frames
import Frames.InCore (RecVec)
import Frames.Rec (Record)
import Frames.ColumnTypeable
import Frames.CSV (readTableOpt, rowGen, RowGen(..))
import Pipes hiding (Proxy)
import Lens.Micro.Extras
import Lens.Micro
import Lens.Micro.Type
import qualified Pipes.Prelude as P

-- Data set from http://vincentarelbundock.github.io/Rdatasets/datasets.html
tableTypes "SpamOrHam" "data/SpamAnalysis.csv"

-- someFunc :: IO ()
-- someFunc = putStrLn $ show $ entropy' 15 [7, 4, 4] 2

someFunc :: IO ()
someFunc = show . totalSetEntropy spamClass <$> loadSpamOrHam >>= putStrLn

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


selectUnknownSender :: IO (Frame SpamOrHam)
selectUnknownSender = splitFrameOn unknownSender True <$> loadSpamOrHam

onlySuspiciousWords :: SpamOrHam -> Record '[SuspiciousWords]
onlySuspiciousWords = rcast

uniqueSpam :: IO ([(Text, Int)])
uniqueSpam = do
  soh <- loadSpamOrHam
  spamClassCol <- (\soh' -> F.toList $ view spamClass <$> soh') <$> loadSpamOrHam
  return $ count spamClassCol


-- example usage:
--   F.toList <$> splitFrameOn unknownSender True <$> loadSpamOrHam
-- => IO ([<rows where the UnknownSender column is True>])
splitFrameOn :: (Eq a, Frames.InCore.RecVec rs) =>
             (forall (f :: * -> *).
                Functor f =>
                (a -> f a) -> Record rs -> f (Record rs))
             -> a -> FrameRec rs -> FrameRec rs
splitFrameOn feature value = filterFrame (\r -> (==) (rget feature r) value)

-- example usage:
--     totalSetEntropy spamClass <$> loadSpamOrHam
-- => IO (1.0)
totalSetEntropy :: Ord a => Getting a s a -> Frame s -> Double
totalSetEntropy targetFeature frame =
  entropy (frameLength frame) $ map snd $ count targetFeatureCol
    where targetFeatureCol = F.toList $ view targetFeature <$> frame

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
