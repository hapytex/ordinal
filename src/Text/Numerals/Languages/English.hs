{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Text.Numerals.Languages.English where

import Data.Text(Text)
import Data.Vector(Vector)

import Text.Numerals.Algorithm(NumeralsAlgorithm, numeralsAlgorithm)
import Text.Numerals.Internal(_div10, _rem10, _showText, _mergeWith, _mergeWith')

english :: NumeralsAlgorithm
english = numeralsAlgorithm negativeWord' zeroWord' oneWord' lowWords' midWords' merge'

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

asShortOrdinal :: (Integral i, Show i) => i -> Text
asShortOrdinal n = _showText n <> ordinalSuffix n

towords :: Integral i => i -> Maybe Text
towords 0 = Just "zero"
towords 1 = Just "one"
towords 2 = Just "two"
towords 3 = Just "three"
towords 4 = Just "four"
towords 5 = Just "five"
towords 6 = Just "six"
towords 7 = Just "seven"
towords 8 = Just "eight"
towords 9 = Just "nine"
towords 10 = Just "ten"
-- towords 
towords _ = Nothing

ordinalWords :: Integral i => i -> Text
ordinalWords 1 = "first"
ordinalWords 2 = "second"
ordinalWords 3 = "third"
ordinalWords 4 = "fourth"
ordinalWords 5 = "fifth"
