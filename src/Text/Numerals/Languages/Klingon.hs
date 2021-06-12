{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

{-|
Module      : Text.Numerals.Languages.Klingon
Description : A module to convert numbers to words in the /Klingon/ language.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module contains logic to convert numbers to words in the /Klingon/ language.
-}

-- TODO: Add extra tests for the language (remove note after adding tests)

module Text.Numerals.Languages.Klingon (
    -- * Num to word algorithm
    klingon
    -- * Convert a cardinal number to text
  , toCardinal'
    -- * Convert to ordinal
  , ordinize'
    -- * Constant words
  , negativeWord', zeroWord', oneWord'
    -- * Names for numbers
  , lowWords', midWords'
    -- * Merge function
  , merge'
  ) where


import Debug.Trace(trace)

import Data.Text(Text, isSuffixOf, snoc)
import qualified Data.Text as T
import Data.Vector(Vector)

import Text.Numerals.Algorithm(HighNumberAlgorithm(LongScale), NumeralsAlgorithm, numeralsAlgorithm)
import Text.Numerals.Algorithm.Template(ordinizeFromDict)
import Text.Numerals.Class(ClockText, FreeMergerFunction, valueSplit, toCardinal)
import Text.Numerals.Internal(_divisable100, _mergeWith, _mergeWithSpace, _mergeWithHyphen, _million, _stripLastIf, _thousand)

-- | A 'NumeralsAlgorithm' to convert numbers to words in the /Klingon/ language.
klingon :: NumeralsAlgorithm  -- ^ A 'NumeralsAlgorithm' that can be used to convert numbers to different formats.
klingon = numeralsAlgorithm negativeWord' zeroWord' oneWord' lowWords' midWords' (const Nothing) merge' ordinize' shortOrdinal' clockText'

-- | Convert numers to their cardinal counterpart in /Klingon/.
toCardinal' :: Integral i
  => i  -- ^ The number to convert to text.
  -> Text  -- ^ The cardinal counterpart in /Klingon/.
toCardinal' = toCardinal klingon

-- | The words used to mark a negative number in the /Klingon/ language.
negativeWord' :: Text
negativeWord' = "pagh bIngDaq"

-- | The word used for the number /zero/ in the /Klingon/ language.
zeroWord' :: Text
zeroWord' = "pagh"

-- | The word used for the number /one/ in the /Klingon/ language.
oneWord' :: Text
oneWord' = "wa’"

-- | A 'Vector' that contains the word used for the numbers /two/ to /nine/ in the /Klingon/ language.
lowWords' :: Vector Text
lowWords' = [
    "cha’"
  , "wej"
  , "loS"
  , "vagh"
  , "jav"
  , "Soch"
  , "chorgh"
  , "Hut"
  ]

-- | A list of 2-tuples that contains the names of values between /thirty/ and
-- /thousand/ in the /Klingon/ language.
midWords' :: [(Integer, Text)]
midWords' = [
    (1000000, "’uy")
  , (100000, "bip")
  , (10000, "netlh")
  , (1000, "SaD")
  , (100, "vatlh")
  , (10, "maH")
  ]

-- | A merge function that is used to combine the names of words together to
-- larger words, according to the /Klingon/ grammar rules.
merge' :: FreeMergerFunction
merge' 1 b | b < 10 = const id
merge' a b | a < b = (<>)
           | otherwise = _mergeWithSpace

-- | A function that converts a number in words in /cardinal/ form to /ordinal/
-- form according to the /Klingon/ language rules.
ordinize' :: Text -> Text
ordinize' = (<> "DIch")

-- | A function to convert a number to its /short ordinal/ form in /Klingon/.
shortOrdinal' :: Integral i
  => i  -- ^ The number to convert to /short ordinal/ form.
  -> Text  -- ^ The equivalent 'Text' specifying the number in /short ordinal/ form.
shortOrdinal' = undefined

-- | Converting the time to a text that describes that time in /Klingon/.
clockText' :: ClockText
-- clockText' _ _ h 0 = toCardinal' h <> "vatlh rep"
-- clockText' _ _ 0 m | m > 0 = "pagh " <> toCardinal' m <> " rep"
clockText' _ _ h m = toCardinal' h <> "maH " <> toCardinal' m <> " rep"
