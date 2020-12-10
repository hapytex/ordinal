{-# LANGUAGE OverloadedLists, OverloadedStrings, TemplateHaskell #-}

{-|
Module      : Text.Numerals.Languages.English
Description : A module to convert numbers to words in the /English/ language.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module contains logic to convert numbers to words in the /English/ language.
-}

module Text.Numerals.Languages.English (
    -- * Num to word algorithm
    english
    -- * Convert a cardinal number to text
  , toCardinal'
    -- * Convert to ordinal
  , ordinize'
    -- * Constant words
  , negativeWord', zeroWord', oneWord'
    -- * Names for numbers
  , lowWords', midWords', highWords'
    -- * Merge function
  , merge'
  ) where

import Data.Default(def)
import Data.Text(Text, isSuffixOf, pack)
import qualified Data.Text as T
import Data.Vector(Vector)

import Text.Numerals.Algorithm(HighNumberAlgorithm, NumeralsAlgorithm, numeralsAlgorithm)
import Text.Numerals.Algorithm.Template(ordinizeFromDict)
import Text.Numerals.Class(ClockText, valueSplit, toCardinal)
import Text.Numerals.Internal(_div10, _mergeWith, _mergeWithSpace, _mergeWithHyphen, _rem10, _showIntegral)

_ordinizepp :: Text -> Text
_ordinizepp t
    | "y" `isSuffixOf` t = T.init t <> "ieth"
    | otherwise = t <> "th"

-- | A function that converts a number in words in /cardinal/ form to /ordinal/
-- form according to the /English/ language rules.
$(pure (ordinizeFromDict "ordinize'" [
    ("one", "first")
  , ("two", "second")
  , ("three", "third")
  , ("four", "fourth")
  , ("five", "fifth")
  , ("six", "sixth")
  , ("seven", "seventh")
  , ("eight", "eighth")
  , ("nine", "ninth")
  , ("ten", "tenth")
  , ("eleven", "eleventh")
  , ("twelve", "twelfth")
  ] '_ordinizepp))

-- | A 'NumeralsAlgorithm' to convert numbers to words in the /English/ language.
english :: NumeralsAlgorithm  -- ^ A 'NumeralsAlgorithm' that can be used to convert numbers to different formats.
english = numeralsAlgorithm negativeWord' zeroWord' oneWord' lowWords' midWords' (valueSplit highWords') merge' ordinize' shortOrdinal' clockText'

-- | Convert numers to their cardinal counterpart in /English/.
toCardinal' :: Integral i
  => i  -- ^ The number to convert to text.
  -> Text  -- ^ The cardinal counterpart in /English/.
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
lowWords' = [
    "two"
  , "three"
  , "four"
  , "five"
  , "six"
  , "seven"
  , "eight"
  , "nine"
  , "ten"
  , "eleven"
  , "twelve"
  , "thirteen"
  , "fourteen"
  , "fifteen"
  , "sixteen"
  , "seventeen"
  , "eighteen"
  , "nineteen"
  , "twenty"
  ]

-- | A list of 2-tuples that contains the names of values between /thirty/ and
-- /thousand/ in the /English/ language.
midWords' :: [(Integer, Text)]
midWords' = [
    (1000, "thousand")
  , (100, "hundred")
  , (90, "ninety")
  , (80, "eighty")
  , (70, "seventy")
  , (60, "sixty")
  , (50, "fifty")
  , (40, "forty")
  , (30, "thirty")
  ]

-- | A merge function that is used to combine the names of words together to
-- larger words, according to the /English/ grammar rules.
merge' :: Integral i => i -> i -> Text -> Text -> Text
merge' 1 r | r < 100 = const id
merge' l r | 100 > l && l > r = _mergeWithHyphen
           | l >= 100 && 100 > r = _mergeWith " and "
           | r > l = _mergeWithSpace
merge' _ _ = _mergeWith ", "

-- | An algorithm to obtain the names of /large/ numbers (one million or larger)
-- in /English/. English uses a /short scale/ with the @illion@ suffix.
highWords' :: HighNumberAlgorithm
highWords' = def

-- | A function to convert a number to its /short ordinal/ form in /English/.
shortOrdinal' :: Integral i
  => i  -- ^ The number to convert to /short ordinal/ form.
  -> Text  -- ^ The equivalent 'Text' specifying the number in /short ordinal/ form.
shortOrdinal' i = pack (_showIntegral i (_shortOrdinalSuffix i))
    where _shortOrdinalSuffix n
              | _rem10 (_div10 n) == 1 = "th"
              | otherwise = go' (_rem10 n)
          go' 1 = "st"
          go' 2 = "nd"
          go' 3 = "rd"
          go' _ = "th"

-- | Converting the time to a text that describes that time in /English/.
clockText' :: ClockText
clockText' cs ds h m = undefined
