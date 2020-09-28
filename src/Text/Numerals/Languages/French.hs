{-# LANGUAGE OverloadedLists, OverloadedStrings, TemplateHaskell #-}

{-|
Module      : Text.Numerals.Languages.French
Description : A module to convert numbers to words in the /French/ language.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module contains logic to convert numbers to words in the /French/ language.
-}

module Text.Numerals.Languages.French (
    -- * Num to word algorithm
    french
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
    ("cinq", "cinqu")
  , ("neuf", "neuv")
  ] 'id])

-- | A 'NumeralsAlgorithm' to convert numbers to words in the /French/ language.
french :: NumeralsAlgorithm  -- ^ A 'NumeralsAlgorithm' that can be used to convert numbers to different formats.
french = numeralsAlgorithm negativeWord' zeroWord' oneWord' lowWords' midWords' (valueSplit highWords') merge' ordinize'

-- | The words used to mark a negative number in the /French/ language.
negativeWord' :: Text
negativeWord' = "moins"

-- | The word used for the number /zero/ in the /French/ language.
zeroWord' :: Text
zeroWord' = "zéro"

-- | The word used for the number /one/ in the /French/ language.
oneWord' :: Text
oneWord' = "un"

-- | A 'Vector' that contains the word used for the numbers /two/ to /twenty/ in the /French/ language.
lowWords' :: Vector Text
lowWords' = [
    "deux"
  , "trois"
  , "quatre"
  , "cinq"
  , "six"
  , "sept"
  , "huit"
  , "neuf"
  , "dix"
  , "onze"
  , "douze"
  , "treize"
  , "quatorze"
  , "quinze"
  , "seize"
  , "dix-sept"
  , "dix-huit"
  , "dix-neuf"
  , "vingt"
  ]

-- | A list of 2-tuples that contains the names of values between /thirty/ and
-- /thousand/ in the /French/ language.
midWords' :: [(Integer, Text)]
midWords' = [
    (1000, "mille")
  , (100, "cent")
  , (80, "quatre-vingts")
  , (60, "soixante")
  , (50, "cinquante")
  , (40, "quarante")
  , (30, "trente")
  ]

-- | A merge function that is used to combine the names of words together to
-- larger words, according to the /French/ grammar rules.
merge' :: FreeMergerFunction
merge' 1 r | r < _million = const id
           | otherwise = _merge' 1 r
merge' l r = \ta tb -> _merge' l r (_firstWithoutS l r ta) (_secondWithS l r tb)

_firstWithoutS :: Integral i => i -> i -> Text -> Text
_firstWithoutS l r t
    | (_divisable100 (l + 20) || (_divisable100 l && l < _thousand)) && r < _million = _stripLastIf 's' t
    | otherwise = t

_secondWithS :: Integral i => i -> i -> Text -> Text
_secondWithS l r t
    | l < _thousand && r /= _thousand && _divisable100 r && not ("s" `isSuffixOf` t) = snoc t 's'
    | otherwise = t

_merge' :: Integral i => i -> i -> Text -> Text -> Text
_merge' l r | r >= l || l >= 100 = _mergeWithSpace
            | r `mod` 10 == 1 && l /= 80 = _mergeWith " et "
            | otherwise = _mergeWithHyphen

-- | A function that converts a number in words in /cardinal/ form to /ordinal/
-- form according to the /French/ language rules.
ordinize' :: Text -> Text
ordinize' "un" = "premier"
ordinize' t = _stripLastIf 'e' (_ordinize' t) <> "ième"

-- | An algorithm to obtain the names of /large/ numbers (one million or larger)
-- in /French/. French uses a /long scale/ with the @illion@ and @illiard@
-- suffixes.
highWords' :: HighNumberAlgorithm
highWords' =  LongScale "illion" "illiard"
