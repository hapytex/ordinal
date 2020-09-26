{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

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

import Data.Text(Text)
import Data.Vector(Vector)

import Text.Numerals.Algorithm(HighNumberAlgorithm(LongScale), NumeralsAlgorithm, numeralsAlgorithm)
import Text.Numerals.Algorithm.Template(ordinizeFromDict)
import Text.Numerals.Class(valueSplit)
import Text.Numerals.Internal(_mergeWith, _mergeWithSpace, _mergeWith')

-- | A 'NumeralsAlgorithm' to convert numbers to words in the /French/ language.
french :: NumeralsAlgorithm  -- ^ A 'NumeralsAlgorithm' that can be used to convert numbers to different formats.
french = numeralsAlgorithm negativeWord' zeroWord' oneWord' lowWords' midWords' (valueSplit highWords') merge' ordinize'

negativeWord' :: Text
negativeWord' = "moins"

zeroWord' :: Text
zeroWord' = "zéro"

oneWord' :: Text
oneWord' = "un"

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

-- TODO
merge' :: Integral i => i -> i -> Text -> Text -> Text
merge' 1 r | r < 100 = const id
merge' l r | 100 > l && l > r = _mergeWith' '-'
           | l >= 100 && 100 > r = _mergeWith " et "
           | r > l = _mergeWithSpace
merge' _ _ = _mergeWithSpace

ordinize' :: Text -> Text
ordinize' t = t

highWords' :: HighNumberAlgorithm
highWords' =  LongScale "illion" "illiard"
