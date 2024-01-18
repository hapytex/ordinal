{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Text.Numerals.Languages.English
-- Description : A module to convert numbers to words in the /English/ language.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains logic to convert numbers to words in the /English/ language.
module Text.Numerals.Languages.English
  ( -- * Num to word algorithm
    english,

    -- * Convert a cardinal number to text
    toCardinal',

    -- * Convert to ordinal
    ordinize',

    -- * Constant words
    negativeWord',
    zeroWord',
    oneWord',

    -- * Names for numbers
    lowWords',
    midWords',
    highWords',

    -- * Merge function
    merge',
  )
where

import Data.Default.Class (Default (def))
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup((<>))
#endif
import Data.Text (Text, isSuffixOf, pack)
import qualified Data.Text as T
import Data.Vector (Vector)
import Text.Numerals.Algorithm (HighNumberAlgorithm, NumeralsAlgorithm, numeralsAlgorithm)
import Text.Numerals.Algorithm.Template (ordinizeFromDict)
import Text.Numerals.Class (ClockSegment (Half, OClock, Past, PastHalf, QuarterPast, QuarterTo, To, ToHalf), ClockText, DayPart (Afternoon, Evening, Morning, Night), DaySegment (dayHour, dayPart), hourCorrection, toCardinal, valueSplit)
import Text.Numerals.Internal (_div10, _mergeWith, _mergeWithHyphen, _mergeWithSpace, _rem10, _showIntegral)

_ordinizepp :: Text -> Text
_ordinizepp t
  | "y" `isSuffixOf` t = T.init t <> "ieth"
  | otherwise = t <> "th"

-- | A function that converts a number in words in /cardinal/ form to /ordinal/
-- form according to the /English/ language rules.
$( pure
     ( ordinizeFromDict
         "ordinize'"
         [ ("one", "first"),
           ("two", "second"),
           ("three", "third"),
           ("four", "fourth"),
           ("five", "fifth"),
           ("six", "sixth"),
           ("seven", "seventh"),
           ("eight", "eighth"),
           ("nine", "ninth"),
           ("ten", "tenth"),
           ("eleven", "eleventh"),
           ("twelve", "twelfth")
         ]
         '_ordinizepp
     )
 )

-- | A 'NumeralsAlgorithm' to convert numbers to words in the /English/ language.
english ::
  -- | A 'NumeralsAlgorithm' that can be used to convert numbers to different formats.
  NumeralsAlgorithm
english = numeralsAlgorithm negativeWord' zeroWord' oneWord' lowWords' midWords' (valueSplit highWords') merge' ordinize' shortOrdinal' clockText'

instance Default NumeralsAlgorithm where
  def = english

-- | Convert numers to their cardinal counterpart in /English/.
toCardinal' ::
  Integral i =>
  -- | The number to convert to text.
  i ->
  -- | The cardinal counterpart in /English/.
  Text
toCardinal' = toCardinal english

-- | The words used to mark a negative number in the /English/ language.
negativeWord' :: Text
negativeWord' = "minus"

-- | The word used for the number /zero/ in the /English/ language.
zeroWord' :: Text
zeroWord' = "zero"

-- | The word used for the number /one/ in the /English/ language.
oneWord' :: Text
oneWord' = "one"

-- | A 'Vector' that contains the word used for the numbers /two/ to /twenty/ in the /English/ language.
lowWords' :: Vector Text
lowWords' =
  [ "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
    "eleven",
    "twelve",
    "thirteen",
    "fourteen",
    "fifteen",
    "sixteen",
    "seventeen",
    "eighteen",
    "nineteen",
    "twenty"
  ]

-- | A list of 2-tuples that contains the names of values between /thirty/ and
-- /thousand/ in the /English/ language.
midWords' :: [(Integer, Text)]
midWords' =
  [ (1000, "thousand"),
    (100, "hundred"),
    (90, "ninety"),
    (80, "eighty"),
    (70, "seventy"),
    (60, "sixty"),
    (50, "fifty"),
    (40, "forty"),
    (30, "thirty")
  ]

-- | A merge function that is used to combine the names of words together to
-- larger words, according to the /English/ grammar rules.
merge' :: Integral i => i -> i -> Text -> Text -> Text
merge' 1 r | r < 100 = const id
merge' l r
  | 100 > l && l > r = _mergeWithHyphen
  | l >= 100 && 100 > r = _mergeWith " and "
  | r > l = _mergeWithSpace
merge' _ _ = _mergeWith ", "

-- | An algorithm to obtain the names of /large/ numbers (one million or larger)
-- in /English/. English uses a /short scale/ with the @illion@ suffix.
highWords' :: HighNumberAlgorithm
highWords' = def

-- | A function to convert a number to its /short ordinal/ form in /English/.
shortOrdinal' ::
  Integral i =>
  -- | The number to convert to /short ordinal/ form.
  i ->
  -- | The equivalent 'Text' specifying the number in /short ordinal/ form.
  Text
shortOrdinal' i = pack (_showIntegral i (_shortOrdinalSuffix i))
  where
    _shortOrdinalSuffix n
      | _rem10 (_div10 n) == 1 = "th"
      | otherwise = go' (_rem10 n)
    go' 1 = "st"
    go' 2 = "nd"
    go' 3 = "rd"
    go' _ = "th"

_dayPartText :: DayPart -> Text
_dayPartText Night = "at night"
_dayPartText Morning = "in the morning"
_dayPartText Afternoon = "in the afternoon"
_dayPartText Evening = "in the evening"

_dayComponent :: Text -> Int -> DaySegment -> Text
_dayComponent sep dh h = toCardinal' (hourCorrection (dayHour h + dh)) <> sep <> _dayPartText (dayPart h)

_dayComponent' :: Int -> DaySegment -> Text
_dayComponent' = _dayComponent " "

-- | Converting the time to a text that describes that time in /English/.
clockText' :: ClockText
clockText' OClock ds _ _ = _dayComponent " o'clock " 0 ds
clockText' (Past m) ds _ _ = toCardinal' m <> " past " <> _dayComponent' 0 ds
clockText' QuarterPast ds _ _ = "quarter past " <> _dayComponent' 0 ds
clockText' (ToHalf _) ds _ m = toCardinal' m <> " past " <> _dayComponent' 0 ds
clockText' Half ds _ _ = "half past " <> _dayComponent' 0 ds
clockText' (PastHalf _) ds _ m = toCardinal' (60 - m) <> " to " <> _dayComponent' 1 ds
clockText' QuarterTo ds _ _ = "quarter to " <> _dayComponent' 1 ds
clockText' (To m) ds _ _ = toCardinal' m <> " to " <> _dayComponent' 1 ds
