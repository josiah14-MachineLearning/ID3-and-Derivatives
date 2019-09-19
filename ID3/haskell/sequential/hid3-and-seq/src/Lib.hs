{-# LANGUAGE TypeOperators,
             DataKinds,
             FlexibleContexts,
             QuasiQuotes,
             TemplateHaskell,
             OverloadedStrings,
             AllowAmbiguousTypes,
             RankNTypes,
             KindSignatures
#-}

module Lib
    ( someFunc
    , entropy'
    , entropy
    , streamSpamOrHam
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
    , findMostInfomativeFeature
    , groupByCol
    ) where
import Control.Foldl as Fl
import Control.Monad.ST
import Control.Monad.Identity
import Control.Applicative hiding (Const)
import Data.Foldable (foldl')
import Data.Foldable as F
import Data.List.Unique
import Data.Map.Strict as M
import qualified Data.Monoid.Inf as I
import Data.Function (on)
import qualified Control.Foldl as L
import Data.Vinyl (rcast)
import Data.Vinyl.Core (rfoldMap, recordToList)
import Data.Vinyl.Functor (getIdentity)
import qualified Data.Vinyl.Functor as VF
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
                           ++ (if l>3 then "\n..." else "")
                             ++ "\nFrame with " ++ (show l) ++ if(l>1) then " rows." else " row."

tableTypes "SpamOrHam" "data/SpamAnalysis.csv"

someFunc :: IO ()
someFunc = do
  frame <- loadSpamOrHam
  putStrLn $ show
    $ informationGain (frameEntropy spamClass frame) frame spamClass
    $ groupByCol unknownSender frame

streamSpamOrHam :: MonadSafe m => P.Producer SpamOrHam m ()
streamSpamOrHam = readTableOpt spamOrHamParser "data/SpamAnalysis.csv"

loadSpamOrHam :: IO (Frame SpamOrHam)
loadSpamOrHam = inCoreAoS streamSpamOrHam

-- First, let's solve this specifically for the SpamOrHam Frame, and then once that's solved, work on
-- a generic solution.
-- [SpamId :-> Int, SuspiciousWords :-> Bool, UnknownSender :-> Bool, Images :-> Bool, SpamClass :-> Text]
findMostInfomativeFeature ::
     (forall f. Functor f => (String -> f String) -> SpamOrHam -> f SpamOrHam)
   -> Record '[
     ((:->) "SpamId" (forall f. Functor f => (Int -> f Int) -> SpamOrHam -> f SpamOrHam)),
     ((:->) "SuspiciousWords" (forall f. Functor f => (Bool -> f Bool) -> SpamOrHam -> f SpamOrHam)),
     ((:->) "UnknownSender" (forall f. Functor f => (Bool -> f Bool) -> SpamOrHam -> f SpamOrHam)),
     ((:->) "Images" (forall f. Functor f => (Bool -> f Bool) -> SpamOrHam -> f SpamOrHam))
   ]
   -> Frame SpamOrHam -> I.NegInf Double
findMostInfomativeFeature tgtFeat descFeats frame =
  rfoldMap (\colacc -> --undefined)
             (I.Finite
             $ informationGain (frameEntropy tgtFeat frame) frame tgtFeat
             $ groupByCol (getIdentity colacc) frame) :: I.NegInf Double) --type erasure, here???
  descFeats


-- Pass the grouped frame in instead of the descriptiveFeature, and then
-- run through the list of descriptiveFeatures available while using the
-- ST monad to keep track of the min as the algo progresses through
-- the list.
informationGain :: (Ord a, RecVec rs) =>
                Double
             -> FrameRec rs
             -> (forall f. Functor f => (a -> f a) -> Record rs -> f (Record rs))
             -> Map b (FrameRec rs)
             -> Double
informationGain originalEntropy frame targetFeature groupedFrames =
  originalEntropy - remainingEntropy frame targetFeature groupedFrames

remainingEntropy :: (Ord a, RecVec rs) =>
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

groupByCol :: (Ord a, RecVec rs) =>
             (forall f. Functor f => (a -> f a) -> Record rs -> f (Record rs))
             -> FrameRec rs -> Map a (FrameRec rs)
groupByCol feature frame =
  M.map mkFrame $ F.foldl' groupBy M.empty [0..(frameLength frame - 1)]
    where
      mkFrame is = Frame (V.length is) $ frameRow frame . (V.!) is
      groupBy m i =
        M.insertWith (V.++) (view feature $ frameRow frame i) (V.singleton i) m

-- example usage:
--     frameEntropy spamClass <$> loadSpamOrHam
-- => IO (1.0)
frameEntropy :: Ord a => Getting a s a -> Frame s -> Double
frameEntropy targetFeature frame =
  entropy (frameLength frame) $ fmap snd $ count targetFeatureCol
    where targetFeatureCol = F.toList $ view targetFeature <$> frame

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
