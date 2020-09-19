{-# LANGUAGE RankNTypes #-}

module Text.Numerals.Algorithm where

import Data.Foldable(toList)
import Data.List(sortOn)
import Data.Maybe(maybe)
import Data.Text(Text, cons)
import Data.Vector(Vector, (!), fromList)
import qualified Data.Vector as V

type MergerFunction i = i -> i -> Text -> Text -> Text

data NumeralsAlgorithm = NumeralsAlgorithm {
    minusWord :: Text
  , oneWord :: Text
  , lowWords :: Vector Text
  , midWords :: [(Int, Text)]
  , mergeFunction :: forall i . Integral i => MergerFunction i
  }

numeralsAlgorithm :: (Foldable f, Foldable g) => Text -> Text -> Text -> f Text -> g (Int, Text) -> (forall i . Integral i => MergerFunction i) -> NumeralsAlgorithm
numeralsAlgorithm minus zero one lowWords midWords = NumeralsAlgorithm minus one (fromList (zero : one : toList lowWords)) ((sortOn (negate . fst) . toList) midWords)

data NumberSegment i = NumberSegment {
      segmentDivision :: MNumberSegment i
    , segmentValue :: i
    , segmentText :: Text
    , segmentRemainder ::  MNumberSegment i
    } deriving (Eq, Ord, Read, Show)

type MNumberSegment i = Maybe (NumberSegment i)

maybeSegment :: Integral i => (i -> NumberSegment i) -> i -> MNumberSegment i
maybeSegment f = go
    where go 0 = Nothing
          go i = Just (f i)

toSegmentLow' :: Integral i => Vector Text -> i -> NumberSegment i
toSegmentLow' vs = go
    where go i | i >= nvs = NumberSegment (Just (go dv)) nvs lv (tl md)
               | otherwise = NumberSegment Nothing i (vs ! fromIntegral i) Nothing
               where (dv, md) = divMod i nvs
          lv = V.last vs
          nvs = fromIntegral (V.length vs) - 1
          tl = maybeSegment go

toSegments' :: Integral i => Vector Text -> [(Int, Text)] -> i -> NumberSegment i
toSegments' lows = go
    where go [] n = toSegmentLow' lows n
          go ma@((m, v) : ms) n
              | im > n = go ms n
              | dv == 1 = NumberSegment Nothing im v tl
              | otherwise = NumberSegment (Just (go ma dv)) im v tl
              where im = fromIntegral m
                    (dv, md) = divMod n im
                    tl = maybeSegment (go ms) md

compressSegments' :: Integral i => Text -> MergerFunction i -> NumberSegment i -> Text
compressSegments' one' merger = snd . go
    where go (NumberSegment dv' i t md') = _mergeTail md' (dvi * i, merger dvi i dv t)
              where (dvi, dv) = _unwrap dv'
          _unwrap = maybe (1, one') go
          _mergeTail Nothing r = r
          _mergeTail (Just md') (vi, v) = (vi + mdi, merger vi mdi v md)
              where (mdi, md) = go md'

toCardinal :: Integral i => NumeralsAlgorithm -> i -> Text
toCardinal NumeralsAlgorithm { minusWord=minusWord, oneWord=oneWord, lowWords=lowWords, midWords=midWords, mergeFunction=mergeFunction } = cardinal
    where cardinal i
              | i < 0 = minusWord <> cons ' ' (go (-i))
              | otherwise = go i
              where go = compressSegments' oneWord mergeFunction . toSegments' lowWords midWords
