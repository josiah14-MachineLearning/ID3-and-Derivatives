-- from Frames.CSV.hs
{-# LANGUAGE 
            BangPatterns, 
            CPP, 
            DataKinds, 
            FlexibleInstances,
            KindSignatures, 
            LambdaCase, 
            MultiParamTypeClasses,
            OverloadedStrings, 
            QuasiQuotes, 
            RankNTypes,
            RecordWildCards, 
            ScopedTypeVariables, 
            TemplateHaskell,
            TypeOperators #-}

module TableTypesList (tableTypesList) where

-- Copied from Fames source
import Control.Arrow (first, second)
import Control.Monad (when, void)
import Data.Char (isAlpha, isAlphaNum, toLower, toUpper)
import qualified Data.Foldable as F
import Data.List (intercalate)
import Data.Maybe (isNothing, fromMaybe)
import Data.Monoid ((<>))
import Data.Proxy
import qualified Data.Text as T
import Data.Vinyl (RElem, Rec)
import Data.Vinyl.TypeLevel (RecAll, RIndex)
import Data.Vinyl.Functor (Identity)
import Frames.Col
import Frames.ColumnTypeable
import Frames.ColumnUniverse
import Frames.Rec
import Frames.RecF
import Frames.RecLens
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Pipes ((>->))
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.Safe as P
import qualified Pipes.Safe.Prelude
import System.IO (IOMode(ReadMode))

import Frames.Frame
import Frames.CSV ( colDec
                  , prefixSize
                  , sanitizeTypeName
                  , readColHeaders
                  , rowTypeName
                  , recDec
                  , rowGen
                  , ParserOptions(..)
                  , QuotingMode(RFC4180Quoting)
                  , RowGen(..)
                  )


tableTypesList' :: forall a. (ColumnTypeable a, Monoid a)
                        => RowGen a -> DecsQ
tableTypesList' (RowGen {..}) =
    do  -- Read column heads from file
        headers <- runIO . P.runSafeT $ readColHeaders opts lineSource
        -- Declare the provided row type name as a synonym of the file-derived record type 
        recTy <- tySynD (mkName rowTypeName) [] (recDec' headers)
        -- Create hamOrSpamParser value name
        let [optsName, colNamesName, colStrsName, colLoopFuncName] = case rowTypeName of
                                        [] -> error "Row type name shouldn't be empty"
                                        h:t -> 
                                            let lowcase = toLower h : t 
                                            in map (mkName . (lowcase++)) ["Parser", "ColNames", "ColStrs", "ColLoop"]
        -- Declare hamOrSpamParser type signature
        optsTy <- sigD optsName [t|ParserOptions|]
        -- Declare hamOrSpamParser based on column names
        optsDec <- valD (varP optsName) (normalB $ lift opts) []
        -- Declare column types 
        colDecs <- concat <$> mapM (uncurry mkColDecs) headers
        -- Declare name list type
        colNamesTy <- sigD colNamesName [t| [Name] |]
        colStrsTy <- sigD colStrsName [t| [String] |]
        -- Extract names from mkColDecs as Names
        let colNames = extractColNames colDecs
        -- Tranform names to expressions
        let colNameExps = map (varE) colNames
        -- Transform names to string literal expressions
        let colLitExps = map (litE . stringL . nameBase) colNames
        -- Transform names to mkName calls
        let colNamegenExps = map (appE (varE 'mkName)) colLitExps
        -- Declare lists of names and name strings
        colStrsDec <- valD (varP colStrsName) (normalB $ listE colLitExps) []
        colNamesDec <- valD (varP colNamesName) (normalB $ listE colNamegenExps) []
        -- Declare column loop function
        colLoopTy <- sigD colLoopFuncName [t| forall rs b. ((forall f a. Functor f => (a -> f a) -> Record rs -> f (Record rs)) -> b)  -> [b] |]
        colLoopDec <- funD colLoopFuncName [clause [[p| func |]] (normalB $ listE [[e| func $colNameExp |] | colNameExp <- colNameExps]) []]
        -- Consolidate declarations into list 
        return (recTy : optsTy : optsDec : colNamesTy : colNamesDec : colStrsTy : colStrsDec : colLoopTy : colLoopDec : colDecs)
    where   recDec' = recDec . map (second colType) :: [(T.Text, a)] -> Q Type
            colNames' | null columnNames = Nothing
                      | otherwise = Just (map T.pack columnNames)
            opts = ParserOptions colNames' separator (RFC4180Quoting '\"')
            lineSource = lineReader >-> P.take prefixSize
            mkColDecs colNm colTy = do
                let safeName = tablePrefix ++ (T.unpack . sanitizeTypeName $ colNm)
                mColNm <- lookupTypeName safeName
                case mColNm of
                    Just _ -> pure []
                    Nothing -> colDec (T.pack tablePrefix) colNm colTy
            extractColNames [] = []
            extractColNames ds = (extractColName init) : (extractColNames tail)
                where (init, tail) = splitAt 5 ds
                      extractColName [_, _, (ValD (VarP name) _ _), _, _] = name


tableTypesList :: String -> FilePath -> DecsQ
tableTypesList n fp = tableTypesList' (rowGen fp) { rowTypeName = n }

-- This works:
--Prelude> :set -XRankNTypes
--Prelude> let g = (\f -> [f [1,2,3], f ["a", "b", "c"]]) :: (forall a . [a] -> b) -> [b]
--Prelude> g length
--[3,3]
--
--But it doesn't work if the signature is removed because the parameter `a` if fixed as constant for rach polymorphic instation of g.
-- I was trying to use TH to generate a function
-- spamOrHamColLoop :: ((forall f a. Functor f => (a -> f a) -> Record rs -> f (Record rs)) -> b) -> [b] 
-- i.e. the parameter is function that always returns b and accepts any column lens as input.
-- But the `a` parameter seems to lose a level of polymorphism in TH.
-- Intended usage:
-- do
--  frame <- loadSpamOrHam
--  let infGains = spamOrHamColLoop (\feature -> informationGain frame feature (groupByCol feature frame))
--  print infGains
 

