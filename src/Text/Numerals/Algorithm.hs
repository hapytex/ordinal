{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Text.Numerals.Algorithm
-- Description : A module that contains functions to construct algorithmic conversions from numbers to words.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A module that contains data types and functions to automatically convert a number to words. It has tooling for a 'NumeralsAlgorithm'
-- as well as a 'HighNumberAlgorithm' that is used to generate a 'ShortScale' or 'LongScale'.
module Text.Numerals.Algorithm
  ( -- * Data types for number algorithms
    NumeralsAlgorithm,
    numeralsAlgorithm,

    -- * Large number algorithms
    HighNumberAlgorithm (ShortScale, LongScale),
    shortScale,
    longScale,
    shortScaleTitle,
    longScaleTitle,
    valueSplit',

    -- * Conversion to a 'NumberSegment'
    toSegments,
    toSegmentLow,
    toSegmentMid,
    toSegmentHigh,

    -- * Segment compression
    compressSegments,
  )
where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Default.Class (Default (def))
import Data.Foldable (toList)
import Data.List (sortOn)
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup((<>))
#endif
import Data.Text (Text, cons, toTitle)
import Data.Vector (Vector, fromList, (!), (!?))
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Test.QuickCheck (oneof)
import Test.QuickCheck.Arbitrary (Arbitrary (arbitrary, shrink))
import Text.Numerals.Class
  ( ClockText,
    FreeMergerFunction,
    FreeNumberToWords,
    FreeValueSplitter,
    MNumberSegment,
    MergerFunction,
    NumToWord (toCardinal, toOrdinal, toShortOrdinal, toTimeText'),
    NumberSegment (NumberSegment),
    NumberSegmenting,
    ValueSplit (valueSplit),
    ValueSplitter,
    toClockSegment,
    toDaySegment,
  )
import Text.Numerals.Internal (_genText, _iLogFloor, _shrinkText, _thousand)
import Text.Numerals.Prefix (latinPrefixes)

-- | A data type for algorithmic number to word conversions. Most western
-- languages /likely/ can work with this data type.
data NumeralsAlgorithm = NumeralsAlgorithm
  { -- | The word used as prefix to denote negative numbers.
    minusWord :: Text,
    -- | The word used to denote /one/ in the language.
    oneWord :: Text,
    -- | A 'Vector' of small numbers, the first item is the word for /two/ and each successor is the word for the next number.
    lowWords :: Vector Text,
    -- | A list of 2-tuples where the first item contains the value, and the second the corresponding word, the values are ordered in descending value order.
    midWords :: [(Integer, Text)],
    -- | A function that is used to generate words for large values (greater than or equal to one /million/), often constructed with the /short scale/ or /long scale/.
    highWords :: FreeValueSplitter,
    -- | A function that specifies how to merge words based on the grammar of that specific language.
    mergeFunction :: FreeMergerFunction,
    -- | A function to conver the /cardinal/ form of a number in an /ordinal/ one.
    ordinize :: Text -> Text,
    -- | A function that converts a number to its /short ordinal/ form.
    shortOrdinal :: FreeNumberToWords,
    -- | A function that converts the clock segment and day segment to a /Text/ that describes the time of the day in words.
    clockText :: ClockText
  }

instance NumToWord NumeralsAlgorithm where
  toCardinal NumeralsAlgorithm {minusWord = _minusWord, oneWord = _oneWord, lowWords = _lowWords, midWords = _midWords, highWords = _highWords, mergeFunction = _mergeFunction} = cardinal
    where
      cardinal i
        | i < 0 = _minusWord <> cons ' ' (go (-j))
        | otherwise = go j
        where
          go = compressSegments _oneWord _mergeFunction . toSegments _lowWords _midWords _highWords
          j = fromIntegral i :: Integer

  toOrdinal na@NumeralsAlgorithm {ordinize = _ordinize} = _ordinize . toCardinal na
  toShortOrdinal = shortOrdinal
  toTimeText' alg h m = clockText alg (toClockSegment m) (toDaySegment h) h m

_toNumberScale :: (Integral i, Integral j) => i -> (j, i)
_toNumberScale i = (l, k)
  where
    ~(_, l, k) = _iLogFloor _thousand i

-- | A data type used for to map larger numbers to words. This data type
-- supports the /short scale/ and /long scale/ with /Latin/ prefixes, and
-- custom suffixes. The 'Default' value is the /short scale/ with /illion/
-- as suffix. This is used in /English/ for large numbers.
data HighNumberAlgorithm
  = ShortScale Text
  | LongScale Text Text
  deriving (Data, Eq, Generic, Ord, Read, Show)

instance NFData HighNumberAlgorithm

instance Arbitrary HighNumberAlgorithm where
  arbitrary = oneof [ShortScale <$> _genText, LongScale <$> _genText <*> _genText]
  shrink (ShortScale t) = ShortScale <$> _shrinkText t
  shrink (LongScale ta tb) = ((`LongScale` tb) <$> _shrinkText ta) <> (LongScale ta <$> _shrinkText tb)

instance Default HighNumberAlgorithm where
  def = ShortScale "illion"

-- | Construct a 'FreeValueSplitter' function for the given suffix for a /short scale/.
shortScale :: Text -> FreeValueSplitter
shortScale = valueSplit . ShortScale

-- | Construct a 'FreeValueSplitter' function for the given suffixes for a /long scale/.
longScale :: Text -> Text -> FreeValueSplitter
longScale suf1 = valueSplit . LongScale suf1

-- | Construct a 'FreeValueSplitter' function for the given suffix for a /short scale/, the names are written in /title case/.
shortScaleTitle :: Text -> FreeValueSplitter
shortScaleTitle = valueSplit' toTitle . ShortScale

-- | Construct a 'FreeValueSplitter' function for the given suffixes for a /long scale/, the names are written in /title case/.
longScaleTitle :: Text -> Text -> FreeValueSplitter
longScaleTitle suf1 = valueSplit' toTitle . LongScale suf1

_highWithSuffix :: Text -> Int -> Maybe Text
_highWithSuffix suf = fmap (<> suf) . (latinPrefixes !?)

_highToText :: HighNumberAlgorithm -> Int -> Maybe Text
_highToText (ShortScale suf) j = _highWithSuffix suf j
_highToText (LongScale suf1 suf2) j
  | even j = _highWithSuffix suf1 k
  | otherwise = _highWithSuffix suf2 k
  where
    k = div j 2

-- | Generate a /value splitter/ for a 'HighNumberAlgorithm' but where the result
-- is post-processed by a function.
valueSplit' ::
  -- | The post-processing function.
  (Text -> Text) ->
  -- | The 'HighNumberAlgorithm' that is used.
  HighNumberAlgorithm ->
  -- | The 'FreeValueSplitter' result.
  FreeValueSplitter
valueSplit' f vs i = (m,) . f <$> _highToText vs (j - 2)
  where
    ~(j, m) = _toNumberScale i

instance ValueSplit HighNumberAlgorithm where
  valueSplit = valueSplit' id

-- | A /smart constructor/ for the 'NumeralsAlgorithm' type. This constructor
-- allows one to use an arbitrary 'Foldable' type for the low words and mid
-- words. It will also order the midwords accordingly.
numeralsAlgorithm :: (Foldable f, Foldable g) => Text -> Text -> Text -> f Text -> g (Integer, Text) -> FreeValueSplitter -> FreeMergerFunction -> (Text -> Text) -> FreeNumberToWords -> ClockText -> NumeralsAlgorithm
numeralsAlgorithm minus zero one _lowWords _midWords = NumeralsAlgorithm minus one (fromList (zero : one : toList _lowWords)) (sortOn (negate . fst) (toList _midWords))

_maybeSegment :: Integral i => (i -> NumberSegment i) -> i -> MNumberSegment i
_maybeSegment f = go
  where
    go 0 = Nothing
    go i = Just (f i)

-- | Convert the given number to a 'NumberSegment' with the given 'Vector' of
-- low numbers. Mid words and large numbers are not taken into account. This
-- is often the next step after the 'toSegmentMid'.
toSegmentLow ::
  Integral i =>
  -- | A 'Vector' of low words.
  Vector Text ->
  -- | The function that maps the number to the 'NumberSegment'.
  NumberSegmenting i
toSegmentLow vs = go
  where
    go i
      | i >= nvs = NumberSegment (Just (go dv)) nvs lv (tl md)
      | otherwise = NumberSegment Nothing i (vs ! fromIntegral i) Nothing
      where
        (dv, md) = divMod i nvs
    lv = V.last vs
    nvs = fromIntegral (V.length vs) - 1
    tl = _maybeSegment go

_splitRecurse :: Integral i => (i -> NumberSegment i) -> (i -> NumberSegment i) -> i -> Text -> i -> NumberSegment i
_splitRecurse f g im v j = NumberSegment hd im v (_maybeSegment g md)
  where
    hd
      | dv == 1 = Nothing
      | otherwise = Just (f dv)
    ~(dv, md) = divMod j im

-- | Convert the given number to a 'NumberSegment' with the given 'Vector' of
-- low numbers, and the /sorted/ list of mid numbers. Large numbers are not
-- taken into account. This is often the next step after the 'toSegmentHigh'.
toSegmentMid ::
  Integral i =>
  -- | A 'Vector' of low words.
  Vector Text ->
  -- | The list of name and the names of these numbers in /descending/ order for the mid words.
  [(Integer, Text)] ->
  -- | The function that maps the number to the 'NumberSegment'.
  NumberSegmenting i
toSegmentMid lows = go
  where
    go [] n = toSegmentLow lows n
    go ma@((m, v) : ms) n
      | im > n = goms n
      | otherwise = _splitRecurse (go ma) goms im v n
      where
        im = fromIntegral m
        goms = go ms

-- | Convert the given number to a 'NumberSegment' with the given 'Vector' of
-- low numbers, the /sorted/ list of mid numbers, and a 'FreeValueSplitter' for
-- large numbers.
toSegmentHigh ::
  Integral i =>
  -- | A 'Vector' of low words.
  Vector Text ->
  -- | The list of name and the names of these numbers in /descending/ order for the mid words.
  [(Integer, Text)] ->
  -- | The 'ValueSplitter' used for large numbers, likely a splitter from a /short scale/ or /long scale/.
  ValueSplitter i ->
  -- | The function that maps the number to the 'NumberSegment'.
  NumberSegmenting i
toSegmentHigh lows mids highs = go
  where
    go v
      | Just (i, t) <- highs v = _splitRecurse go go i t v
      | otherwise = toSegmentMid lows mids v

-- | Convert the given number to a 'NumberSegment' with the given 'Vector' of
-- low numbers, the /sorted/ list of mid numbers, and a 'FreeValueSplitter' for
-- large numbers.
toSegments ::
  Integral i =>
  -- | A 'Vector' of low words.
  Vector Text ->
  -- | The list of name and the names of these numbers in /descending/ order for the mid words.
  [(Integer, Text)] ->
  -- | The 'ValueSplitter' used for large numbers, likely a splitter from a /short scale/ or /long scale/.
  ValueSplitter i ->
  -- | The function that maps the number to the 'NumberSegment'.
  NumberSegmenting i
toSegments = toSegmentHigh

-- | Use the given 'MergerFunction' to compress the 'NumberSegment' to a single
-- 'Text' object that represents the given number.
compressSegments ::
  Integral i =>
  -- | The value used for /one/ in the specific language.
  Text ->
  -- | The 'MergerFunction' for the specific language that implements the grammar rules how to merge values.
  MergerFunction i ->
  -- | The given 'NumberSegment' value to turn into a 'Text' object.
  NumberSegment i ->
  -- | The 'Text' object that contains the name of the number stored in the 'NumberSegment'.
  Text
compressSegments one' merger = snd . go
  where
    go (NumberSegment dv' i t md') = _mergeTail md' (dvi * i, merger dvi i dv t)
      where
        (dvi, dv) = _unwrap dv'
    _unwrap = maybe (1, one') go
    _mergeTail Nothing r = r
    _mergeTail (Just md') (vi, v) = (vi + mdi, merger vi mdi v md)
      where
        (mdi, md) = go md'
