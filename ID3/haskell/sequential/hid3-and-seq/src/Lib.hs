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
    , frameEntropy
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
    , groupByCol
    , groupByCol'
    , groupByCol''
    ) where
import Control.Foldl as Fl
import Control.Monad.ST
import Control.Monad.Identity
import Control.Applicative
import Data.Foldable (foldl')
import Data.Foldable as F
import Data.List.Unique
import Data.Map.Strict as M
import Data.Function (on)
import qualified Control.Foldl as L
import Data.Vinyl (rcast)
import Data.Vinyl.Lens (RElem)
import Data.Vinyl.TypeLevel (RIndex)
import qualified Data.Vector.Unboxed as V
import Frames
import Frames.InCore (RecVec)
import Frames.Rec (Record)
import Frames.ColumnTypeable
import Frames.CSV (readTableOpt, rowGen, RowGen(..))
import Lens.Micro.Extras
import Lens.Micro
import Lens.Micro.Type
import qualified Pipes as P
import qualified Pipes.Prelude as P

 -- define a Show instance for frames
instance (Show a) => Show (Frame a) where
  show (Frame l f) = (show $ f 0)
                       ++ (if l>1 then "\n" ++ (show $ f 1) else "")
                         ++ (if l>2 then "\n..." else "")
                           ++ "\nFrame with " ++ (show l) ++ if(l>1) then " rows." else " row."

-- Data set from http://vincentarelbundock.github.io/Rdatasets/datasets.html
tableTypes "SpamOrHam" "data/SpamAnalysis.csv"

-- someFunc :: IO ()
-- someFunc = putStrLn $ show $ entropy' 15 [7, 4, 4] 2

someFunc :: IO ()
someFunc = do
  frame <- loadSpamOrHam
  putStrLn $ show $ informationGain frame spamClass $ groupByCol suspiciousWords frame

streamSpamOrHam :: MonadSafe m => P.Producer SpamOrHam m ()
streamSpamOrHam = readTableOpt spamOrHamParser "data/SpamAnalysis.csv"

loadSpamOrHam :: IO (Frame SpamOrHam)
loadSpamOrHam = inCoreAoS streamSpamOrHam

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

groupByCol'' :: (Eq a, Ord a, RecVec rs) =>
             (forall f. Functor f => (a -> f a) -> Record rs -> f (Record rs))
             -> FrameRec rs -> Map a (FrameRec rs)
groupByCol'' feature frame = M.map toFrame $ F.foldl' groupBy M.empty frame
  where groupBy m r = M.insertWith (\[new] old -> new:old) (view feature r) [r] m

groupByCol' :: (Eq a, Ord a, RecVec rs) =>
             (forall f. Functor f => (a -> f a) -> Record rs -> f (Record rs))
             -> FrameRec rs -> Map a (FrameRec rs)
groupByCol' feature frame =
  runIdentity $ P.fold groupBy M.empty (M.map toFrame) (P.each frame)
    where groupBy m r = M.insertWith (\[new] old -> new:old) (view feature r) [r] m

-- Pass the grouped frame in instead of the descriptiveFeature, and then
-- run through the list of descriptiveFeatures available while using the
-- ST monad to keep track of the min as the algo progresses through
-- the list.
informationGain :: (Ord a, Eq a, Ord b, Eq b, RecVec rs) =>
                FrameRec rs
             -> (forall f. Functor f => (a -> f a) -> Record rs -> f (Record rs))
             -> Map b (FrameRec rs)
             -> Double
informationGain frame targetFeature groupedFrames =
  (-) (frameEntropy targetFeature frame)
      (remainingEntropy frame targetFeature groupedFrames)

remainingEntropy :: (Ord a, Eq a, Ord b, Eq b, RecVec rs) =>
                FrameRec rs
             -> (forall f. Functor f => (a -> f a) -> Record rs -> f (Record rs))
             -> Map b (FrameRec rs)
             -> Double
remainingEntropy frame targetFeature groupedFrames =
  F.sum $ M.map groupRemEntropy groupedFrames
    where
      (//) = (/) `on` fromIntegral
      groupRemEntropy f =
        frameLength f // frameLength frame * frameEntropy targetFeature f

groupByCol :: (Eq a, Ord a, RecVec rs) =>
             (forall f. Functor f => (a -> f a) -> Record rs -> f (Record rs))
             -> FrameRec rs -> Map a (FrameRec rs)
groupByCol feature frame =
  M.map mkFrame $ F.foldl' groupBy M.empty [0..(frameLength frame - 1)]
    where
      mkFrame is = Frame (V.length is) $ frameRow frame . (V.!) is
      groupBy m i =
        M.insertWith (V.++) (view feature $ frameRow frame i) (V.singleton i) m

-- example usage:
--   F.toList <$> splitFrameOn unknownSender True <$> loadSpamOrHam
-- => IO ([<rows where the UnknownSender column is True>])
splitFrameOn :: (Eq a, RecVec rs) =>
             (forall f. Functor f => (a -> f a) -> Record rs -> f (Record rs))
             -> a -> FrameRec rs -> FrameRec rs
splitFrameOn feature value = filterFrame (\r -> (view feature r) == value)

-- example usage:
--     frameEntropy spamClass <$> loadSpamOrHam
-- => IO (1.0)
frameEntropy :: Ord a => Getting a s a -> Frame s -> Double
frameEntropy targetFeature frame =
  entropy (frameLength frame) $ fmap snd $ count targetFeatureCol
    where targetFeatureCol = F.toList $ view targetFeature <$> frame

-- minMaxIncome :: IO (Maybe Int, Maybe Int)
-- minMaxIncome = (\rows -> L.fold (L.handles income minMax) rows) <$> loadRows
--   where minMax = (,) <$> L.minimum <*> L.maximum

-- ratio :: Record '[Income, Prestige] -> Double
-- ratio = runcurry' (\i p -> fromIntegral i / p)

entropy' :: (Foldable f, Floating b) => Int -> f Int -> Int -> b
entropy' totalElements itemFrequencies logarithmicBase =
  negate $ F.foldl' (\entropyAcc f -> entropyAcc + itemEntropy f) 0 itemFrequencies
    where
      itemEntropy ifreq = let prob = (fromIntegral ifreq) / l
                          in prob * logBase b prob
      l  = fromIntegral totalElements
      b  = fromIntegral logarithmicBase

entropy :: (Foldable f, Floating b) => Int -> f Int -> b
entropy totalElements itemFrequencies = entropy' totalElements itemFrequencies 2
