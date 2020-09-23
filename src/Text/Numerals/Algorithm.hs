{-# LANGUAGE RankNTypes, TemplateHaskellQuotes #-}

module Text.Numerals.Algorithm where

import Control.Applicative(liftA2)

import Data.Foldable(toList)
import Data.List(sortOn)
import Data.Maybe(maybe)
import Data.Text(Text, cons, isSuffixOf, pack, snoc)
import Data.Vector(Vector, (!), fromList)
import qualified Data.Vector as V

import Text.Numerals.Class(NumToWord(toCardinal, toOrdinal), ValueSplit(valueSplit))
import Text.Numerals.Internal(_million, _replaceSuffix, _thousand)
import Text.Numerals.Prefix(greekPrefixes')


type MergerFunction i = i -> i -> Text -> Text -> Text

data NumeralsAlgorithm = NumeralsAlgorithm {
    minusWord :: Text
  , oneWord :: Text
  , lowWords :: Vector Text
  , midWords :: [(Integer, Text)]
  , mergeFunction :: forall i . Integral i => MergerFunction i
  , ordinize :: Text -> Text
  }

instance NumToWord NumeralsAlgorithm where
    toCardinal NumeralsAlgorithm { minusWord=minusWord, oneWord=oneWord, lowWords=lowWords, midWords=midWords, mergeFunction=mergeFunction } = cardinal
       where cardinal i
                  | i < 0 = minusWord <> cons ' ' (go (-i))
                  | otherwise = go i
                  where go = compressSegments' oneWord mergeFunction . toSegments' lowWords midWords

    toOrdinal na@NumeralsAlgorithm { ordinize=ordinize } = ordinize . toCardinal na


data HighNumberAlgorithm
  = ShortScale Text
  | LongScale Text Text

instance ValueSplit HighNumberAlgorithm where
    

numeralsAlgorithm :: (Foldable f, Foldable g, Foldable h) => Text -> Text -> Text -> f Text -> g (Integer, Text) -> h (Integer, Text) -> (forall i . Integral i => MergerFunction i) -> (Text -> Text) -> NumeralsAlgorithm
numeralsAlgorithm minus zero one lowWords midWords highWords = NumeralsAlgorithm minus one (fromList (zero : one : toList lowWords)) (sortOn (negate . fst) (toList midWords ++ toList highWords))

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

toSegments' :: Integral i => Vector Text -> [(Integer, Text)] -> i -> NumberSegment i
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

generatePrefixedHighNumbers :: Integral i => [Text] -> [(i, Text)]
generatePrefixedHighNumbers = reverse . zip (iterate (_thousand*) _million) . liftA2 (<>) greekPrefixes'
