{-# LANGUAGE OverloadedLists, OverloadedStrings, QuasiQuotes, TemplateHaskell #-}

{-|
Module      : Text.Numerals.Languages.German
Description : A module to convert numbers to words in the /German/ language.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module contains logic to convert numbers to words in the /German/ language.
-}

module Text.Numerals.Languages.German (
    -- * Num to word algorithm
    german
    -- * Convert to ordinal
  , ordinize'
    -- * Constant words
  , negativeWord', zeroWord', oneWord'
    -- * Names for numbers
  , lowWords', midWords', highWords'
    -- * Merge function
  , merge'
  ) where

import Data.Bool(bool)
import Data.Text(Text, isSuffixOf, toLower, toTitle)
import Data.Vector(Vector)

import Text.Numerals.Algorithm(HighNumberAlgorithm(LongScale), NumeralsAlgorithm, numeralsAlgorithm, valueSplit')
import Text.Numerals.Algorithm.Template(ordinizeFromDict)
import Text.Numerals.Class(FreeMergerFunction)
import Text.Numerals.Internal(_mergeWith, _mergeWithSpace, _million)
import Text.RE.TDFA.Text(RE, SearchReplace, (*=~/), ed)

$(pure (ordinizeFromDict "_ordinize'" [
    ("eins", "ers")
  , ("drei", "drit")
  , ("acht", "ach")
  , ("sieben", "sieb")
  , ("ig", "igs")
  , ("ert", "erts")
  , ("end", "ends")
  , ("ion", "ions")
  , ("nen", "ns")
  , ("rde", "rds")
  , ("rden", "rds")
  ] 'id))

-- | A 'NumeralsAlgorithm' to convert numbers to words in the /German/ language.
german :: NumeralsAlgorithm  -- ^ A 'NumeralsAlgorithm' that can be used to convert numbers to different formats.
german = numeralsAlgorithm negativeWord' zeroWord' oneWord' lowWords' midWords' (valueSplit' toTitle highWords') merge' ordinize'

-- | The words used to mark a negative number in the /German/ language.
negativeWord' :: Text
negativeWord' = "minus"

-- | The word used for the number /zero/ in the /German/ language.
zeroWord' :: Text
zeroWord' = "null"

-- | The word used for the number /one/ in the /German/ language.
oneWord' :: Text
oneWord' = "eins"

-- | A 'Vector' that contains the word used for the numbers /two/ to /twenty/ in the /German/ language.
lowWords' :: Vector Text
lowWords' = [
    "zwei"
  , "drei"
  , "vier"
  , "fünf"
  , "sechs"
  , "sieben"
  , "acht"
  , "neun"
  , "zehn"
  , "elf"
  , "zwölf"
  , "dreizehn"
  , "vierzehn"
  , "fünfzehn"
  , "sechzehn"
  , "siebzehn"
  , "achtzehn"
  , "neunzehn"
  , "zwanzig"
  ]

-- | A list of 2-tuples that contains the names of values between /thirty/ and
-- /thousand/ in the /German/ language.
midWords' :: [(Integer, Text)]
midWords' = [
    (1000, "tausend")
  , (100, "hundert")
  , (90, "neunzig")
  , (80, "achtzig")
  , (70, "siebzig")
  , (60, "sechzig")
  , (50, "fünfzig")
  , (40, "vierzig")
  , (30, "dreißig")
  ]

-- | A merge function that is used to combine the names of words together to
-- larger words, according to the /German/ grammar rules.
merge' :: FreeMergerFunction
merge' 1 100 = const ("ein" <>)
merge' 1 1000 = const ("ein" <>)
merge' 1 r | r < _million = const id
merge' 1 r = const (_merge' 1 r "eine")
merge' l r = _merge' l r

_pluralize :: Text -> Text
_pluralize t
    | "e" `isSuffixOf` t = t <> "n"
    | otherwise = t <> "en"

_merge' :: FreeMergerFunction
_merge' l r
    | r > l && r >= _million = (. bool id _pluralize (l > 1)) . _mergeWithSpace
    | r > l = (<>)
_merge' l 1 | 10 < l && l < 100 = const . ("einund" <> )
_merge' l r
    | r < 10 && 10 < l && l < 100 = flip (_mergeWith "und")
    | l >= _million = _mergeWithSpace
    | otherwise = (<>)

_ordinalSuffixRe :: SearchReplace RE Text
_ordinalSuffixRe = [ed|(eine)? ([a-z]+(illion|illiard)ste)$///${2}|]

-- | A function that converts a number in words in /cardinal/ form to /ordinal/
-- form according to the /German/ language rules.
ordinize' :: Text -> Text
ordinize' = postprocess . (<> "te") . _ordinize' . toLower
    where postprocess "eintausendste" = "tausendste"
          postprocess "einhundertste" = "hundertste"
          postprocess t = t *=~/ _ordinalSuffixRe

-- | An algorithm to obtain the names of /large/ numbers (one million or larger)
-- in /German/. German uses a /long scale/ with the @illion@ and @illiard@
-- suffixes.
highWords' :: HighNumberAlgorithm
highWords' =  LongScale "illion" "illiard"
