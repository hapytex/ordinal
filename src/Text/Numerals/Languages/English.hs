{-# LANGUAGE OverloadedLists, OverloadedStrings, TemplateHaskell #-}

module Text.Numerals.Languages.English where

import Data.Text(Text, isSuffixOf)
import Data.Vector(Vector)

import Text.Numerals.Algorithm(NumeralsAlgorithm, numeralsAlgorithm, ordinizeFromDict)
import Text.Numerals.Internal(_div10, _rem10, _showText, _mergeWith, _mergeWith', _replaceSuffix)

$(pure [ordinizeFromDict [
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
english = numeralsAlgorithm negativeWord' zeroWord' oneWord' lowWords' midWords' merge' ordinize'

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

midWords' :: [(Int, Text)]
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
merge' l r | 100 > l && l > r = _mergeWith' '-'
           | l >= 100 && 100 > r = _mergeWith " and "
           | r > l = _mergeWith' ' '
merge' _ _ = _mergeWith ", "

--ordinize' t
--     | isSuffixOf "one" t = _replaceSuffix 3 "first" t
--    | isSuffixOf "two" t = _replaceSuffix 3 "second" t
--    | isSuffixOf "three" t = _replaceSuffix 3 "ird" t
--    | isSuffixOf "four" t = t <> "th"
--    | isSuffixOf "five" t = _replaceSuffix 2 "fth" t
--    | isSuffixOf "six" t = t <> "th"
--    | isSuffixOf "seven" t = t <> "th"
--    | isSuffixOf "eight" t = t <> "h"
--    | isSuffixOf "nine" t = _replaceSuffix 1 "th" t
--    | isSuffixOf "ten" t = t <> "th"
--    | isSuffixOf "eleven" t = t <> "th"
--    | isSuffixOf "twelve" t = _replaceSuffix 2 "fth" t
--    | otherwise = t
