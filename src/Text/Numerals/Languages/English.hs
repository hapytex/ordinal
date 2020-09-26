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
    -- * Convert to ordinal
  , ordinize'
    -- * Constant words
  , negativeWord', zeroWord', oneWord'
    -- * Names for numbers
  , lowWords', midWords', highWords'
    -- * Merge function
  , merge'
  ) where

import Data.Text(Text, isSuffixOf)
import Data.Vector(Vector)

import Text.Numerals.Algorithm(HighNumberAlgorithm(ShortScale), NumeralsAlgorithm, numeralsAlgorithm)
import Text.Numerals.Algorithm.Template(ordinizeFromDict)
import Text.Numerals.Class(valueSplit)
import Text.Numerals.Internal(_div10, _rem10, _showText, _mergeWith, _mergeWithSpace, _mergeWithHyphen, _mergeWith', _replaceSuffix)

$(pure [ordinizeFromDict "ordinize'" [
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
  ]])

english :: NumeralsAlgorithm
english = numeralsAlgorithm negativeWord' zeroWord' oneWord' lowWords' midWords' (valueSplit highWords') merge' ordinize'

negativeWord' :: Text
negativeWord' = "minus"

zeroWord' :: Text
zeroWord' = "zero"

oneWord' :: Text
oneWord' = "one"

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

ordinalSuffix :: Integral i => i -> Text
ordinalSuffix n
    | _rem10 (_div10 n) == 1 = "th"
    | otherwise = go (_rem10 n)
    where go 1 = "st"
          go 2 = "nd"
          go 3 = "rd"
          go _ = "th"

merge' :: Integral i => i -> i -> Text -> Text -> Text
merge' 1 r | r < 100 = const id
merge' l r | 100 > l && l > r = _mergeWithHyphen
           | l >= 100 && 100 > r = _mergeWith " and "
           | r > l = _mergeWithSpace
merge' _ _ = _mergeWith ", "

highWords' :: HighNumberAlgorithm
highWords' = ShortScale "illion"
