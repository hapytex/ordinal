{-# LANGUAGE OverloadedLists, OverloadedStrings, TemplateHaskell #-}

{-|
Module      : Text.Numerals.Languages.German
Description : A module to convert numbers to words in the /German/ language.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module contains logic to convert numbers to words in the /German/ language.
-}

-- TODO: Add extra tests for the language (remove note after adding tests)

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

import Data.Text(Text, isSuffixOf, snoc)
import qualified Data.Text as T
import Data.Vector(Vector)

import Text.Numerals.Algorithm(HighNumberAlgorithm(LongScale), NumeralsAlgorithm, numeralsAlgorithm)
import Text.Numerals.Algorithm.Template(ordinizeFromDict)
import Text.Numerals.Class(FreeMergerFunction, valueSplit)
import Text.Numerals.Internal(_divisable100, _mergeWith, _mergeWithSpace, _mergeWithHyphen, _million, _stripLastIf, _thousand)

$(pure [ordinizeFromDict "_ordinize'" [
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
  ] 'id])

-- | A 'NumeralsAlgorithm' to convert numbers to words in the /German/ language.
german :: NumeralsAlgorithm  -- ^ A 'NumeralsAlgorithm' that can be used to convert numbers to different formats.
german = numeralsAlgorithm negativeWord' zeroWord' oneWord' lowWords' midWords' (valueSplit highWords') merge' ordinize'

-- | The words used to mark a negative number in the /German/ language.
negativeWord' :: Text
negativeWord' = "minus"

-- | The word used for the number /zero/ in the /German/ language.
zeroWord' :: Text
zeroWord' = "null"

-- | The word used for the number /one/ in the /German/ language.
oneWord' :: Text
oneWord' = "eins"

-- | A 'Vector' that contains the word used for the numbers /two/ to /???/ in the /???/ language.
lowWords' :: Vector Text
lowWords' = [
    "zwei"
  , "drie"
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
-- /thousand/ in the /???/ language.
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
-- larger words, according to the /???/ grammar rules.
merge' :: FreeMergerFunction
merge' 1 100 = const ("ein" <>)
merge' 1 1000 = const ("ein" <>)
merge' 1 r | r < _million = const id
merge' 1 r = const (_merge' 1 r "eine")
merge' l r = _merge' l r

_merge' :: FreeMergerFunction
_merge' l r = (<>)
_merge' l r = _mergeWithSpace

-- | A function that converts a number in words in /cardinal/ form to /ordinal/
-- form according to the /???/ language rules.
ordinize' :: Text -> Text
ordinize' = postprocess . (<> "te") . _ordinize'
    where postprocess "eintausendste" = "tausendste"
          postprocess "einhundertste" = "hundertste"
          -- TODO: millionste/miljardste
          postprocess t = t

-- | An algorithm to obtain the names of /large/ numbers (one million or larger)
-- in /???/. ??? uses a /long scale/ with the @???@ and @???@
-- suffixes.
highWords' :: HighNumberAlgorithm
highWords' =  LongScale "illion" "illiarde"