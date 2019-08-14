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
    , groupByIdx
    , groupByCol
    ) where
import Control.Foldl as Fl
import Control.Monad.ST
import Data.Foldable (foldl')
import Data.Foldable as F
import Data.List.Unique
import Data.Map.Strict as M
import qualified Control.Foldl as L
import Data.Vinyl (rcast)
import Data.Vinyl.Lens (RElem)
import Data.Vinyl.TypeLevel (RIndex)
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
someFunc = show . totalSetEntropy spamClass <$> loadSpamOrHam >>= putStrLn

streamSpamOrHam :: MonadSafe m => P.Producer SpamOrHam m ()
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

-- ex: putStrLn $ show $ groupByIdx [1,2,3,4,1,2,1,2,4,2,1,2,3,2,1]
-- => fromList [(1,[14,10,6,4,0]),(2,[13,11,9,7,5,1]),(3,[12,2]),(4,[8,3])]
groupByIdx :: Ord a => [a] -> Map a [Int]
groupByIdx = groupByIdx' 0 M.empty

groupByIdx' :: Ord a => Int -> Map a [Int] -> [a] -> Map a [Int]
groupByIdx' _ idxs [] = idxs
groupByIdx' i idxs (v:vs) = groupByIdx' (i+1) idxs' vs
    where idxs' = M.insertWith (\[new] old -> new:old) v [i] idxs

groupByCol :: (Eq a, Ord a, RecVec rs) =>
             (forall (f :: * -> *).
                 Functor f =>
                 (a -> f a) -> Record rs -> f (Record rs))
             -> FrameRec rs -> Map a (FrameRec rs)
groupByCol feature frame = M.map toFrame $ F.foldl' groupBy M.empty frame
  where groupBy m r = M.insertWith (\[new] old -> new:old) (view feature r) [r] m

-- I couldn't figure out how to translate the above function that uses regular Foldl
-- into the below function that uses the Pipes API.  There's too much in that library
-- that I don't understand well enough to decipher exactly what's wrong with my
-- current implementation.
groupByCol' :: (Eq a, Ord a, RecVec rs) =>
             (forall (f :: * -> *).
                 Functor f =>
                 (a -> f a) -> Record rs -> f (Record rs))
             -> FrameRec rs -> Map a (FrameRec rs)
groupByCol' feature frame =
  P.fold groupBy M.empty toFrame (P.each frame)
    where groupBy m r = M.insertWith (\[new] old -> new:old) (view feature r) [r] m


-- example usage:
--   F.toList <$> splitFrameOn unknownSender True <$> loadSpamOrHam
-- => IO ([<rows where the UnknownSender column is True>])
splitFrameOn :: (Eq a, RecVec rs) =>
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
