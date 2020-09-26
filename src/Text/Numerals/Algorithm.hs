{-# LANGUAGE RankNTypes, TupleSections #-}

module Text.Numerals.Algorithm where

import Control.Applicative(liftA2)

import Data.Foldable(toList)
import Data.List(sortOn)
import Data.Maybe(maybe)
import Data.Text(Text, cons, isSuffixOf, pack, snoc)
import Data.Vector(Vector, (!), fromList)
import qualified Data.Vector as V

import Text.Numerals.Class(NumToWord(toCardinal, toOrdinal), ValueSplit(valueSplit))
import Text.Numerals.Internal(_million, _replaceSuffix, _thousand, _iLogFloor)
import Text.Numerals.Prefix(latinPrefix)


type MergerFunction i = i -> i -> Text -> Text -> Text
type FreeMergerFunction = forall i . Integral i => MergerFunction i
type ValueSplitter i = i -> Maybe (i, Text)
type FreeValueSplitter = forall i . Integral i => ValueSplitter i
type NumberSegmenting i = i -> NumberSegment i

data NumeralsAlgorithm = NumeralsAlgorithm {
    minusWord :: Text
  , oneWord :: Text
  , lowWords :: Vector Text
  , midWords :: [(Integer, Text)]
  , highWords :: forall i . Integral i => i -> Maybe (i, Text)
  , mergeFunction :: forall i . Integral i => MergerFunction i
  , ordinize :: Text -> Text
  }


instance NumToWord NumeralsAlgorithm where
    toCardinal NumeralsAlgorithm { minusWord=minusWord, oneWord=oneWord, lowWords=lowWords, midWords=midWords, highWords=highWords, mergeFunction=mergeFunction } = cardinal
       where cardinal i
                  | i < 0 = minusWord <> cons ' ' (go (-i))
                  | otherwise = go i
                  where go = compressSegments' oneWord mergeFunction . toSegments lowWords midWords highWords

    toOrdinal na@NumeralsAlgorithm { ordinize=ordinize } = ordinize . toCardinal na


_toNumberScale :: (Integral i, Integral j) => i -> (j, i)
_toNumberScale i = (l, k)
    where ~(_, l, k) = _iLogFloor _thousand i

data HighNumberAlgorithm
  = ShortScale Text
  | LongScale Text Text

shortScale :: Text -> FreeValueSplitter
shortScale = valueSplit . ShortScale

longScale :: Text -> Text -> FreeValueSplitter
longScale suf1 = valueSplit . LongScale suf1

_highWithSuffix :: Text -> Int -> Maybe Text
_highWithSuffix suf = fmap (<> suf) . latinPrefix

_highToText :: HighNumberAlgorithm -> Int -> Maybe Text
_highToText (ShortScale suf) j = _highWithSuffix suf j
_highToText (LongScale suf1 suf2) j
    | even j = _highWithSuffix suf1 k
    | otherwise = _highWithSuffix suf2 k
    where k = div j 2

instance ValueSplit HighNumberAlgorithm where
    valueSplit vs i = go
        where go | j < 0 = Nothing
                 | otherwise = (m,) <$> _highToText vs j
              ~(j, m) = _toNumberScale i

numeralsAlgorithm :: (Foldable f, Foldable g) => Text -> Text -> Text -> f Text -> g (Integer, Text) -> FreeValueSplitter -> FreeMergerFunction -> (Text -> Text) -> NumeralsAlgorithm
numeralsAlgorithm minus zero one lowWords midWords = NumeralsAlgorithm minus one (fromList (zero : one : toList lowWords)) (sortOn (negate . fst) (toList midWords))

data NumberSegment i = NumberSegment {
      segmentDivision :: MNumberSegment i
    , segmentValue :: i
    , segmentText :: Text
    , segmentRemainder ::  MNumberSegment i
    } deriving (Eq, Ord, Read, Show)

type MNumberSegment i = Maybe (NumberSegment i)

_maybeSegment :: Integral i => (i -> NumberSegment i) -> i -> MNumberSegment i
_maybeSegment f = go
    where go 0 = Nothing
          go i = Just (f i)

toSegmentLow' :: Integral i => Vector Text -> i -> NumberSegment i
toSegmentLow' vs = go
    where go i | i >= nvs = NumberSegment (Just (go dv)) nvs lv (tl md)
               | otherwise = NumberSegment Nothing i (vs ! fromIntegral i) Nothing
               where (dv, md) = divMod i nvs
          lv = V.last vs
          nvs = fromIntegral (V.length vs) - 1
          tl = _maybeSegment go

_splitRecurse :: Integral i => (i -> NumberSegment i) -> (i -> NumberSegment i) -> i -> Text -> i -> NumberSegment i
_splitRecurse f g im v j = NumberSegment hd im v (_maybeSegment g md)
    where hd | dv == 1 = Nothing
             | otherwise = Just (f dv)
          ~(dv, md) = divMod j im

toSegmentMid' :: Integral i => Vector Text -> [(Integer, Text)] -> i -> NumberSegment i
toSegmentMid' lows = go
    where go [] n = toSegmentLow' lows n
          go ma@((m, v) : ms) n
              | im > n = goms n
              | otherwise = _splitRecurse (go ma) goms im v n
              where im = fromIntegral m
                    goms = go ms

toSegmentHigh' :: Integral i => Vector Text -> [(Integer, Text)] -> FreeValueSplitter -> NumberSegmenting i
toSegmentHigh' lows mids highs = go
    where go v | Just (i, t) <- highs v = _splitRecurse go go i t v
               | otherwise = toSegmentMid' lows mids v

toSegments :: Integral i => Vector Text -> [(Integer, Text)] -> FreeValueSplitter -> NumberSegmenting i
toSegments = toSegmentHigh'

compressSegments' :: Integral i => Text -> MergerFunction i -> NumberSegment i -> Text
compressSegments' one' merger = snd . go
    where go (NumberSegment dv' i t md') = _mergeTail md' (dvi * i, merger dvi i dv t)
              where (dvi, dv) = _unwrap dv'
          _unwrap = maybe (1, one') go
          _mergeTail Nothing r = r
          _mergeTail (Just md') (vi, v) = (vi + mdi, merger vi mdi v md)
              where (mdi, md) = go md'
