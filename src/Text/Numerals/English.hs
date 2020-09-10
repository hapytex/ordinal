{-# LANGUAGE OverloadedStrings, Safe #-}

module Text.Numerals.English where

import Data.Text(Text)

import Text.Numerals.Internal(_div10, _rem10, _showText)

negative_word :: Text
negative_word = "minus"

ordinalSuffix :: Integral i => i -> Text
ordinalSuffix n
    | _rem10 (_div10 n) == 1 = "th"
    | otherwise = go (_rem10 n)
    where go 1 = "st"
          go 2 = "nd"
          go 3 = "rd"
          go _ = "th"

asShortOrdinal :: (Integral i, Show i) => i -> Text
asShortOrdinal n = _showText n <> ordinalSuffix n

towords :: Integral i => i -> Text
towords 0 = "zero"
towords 1 = "one"
towords 2 = "two"
towords 3 = "three"
towords 4 = "four"
towords 5 = "five"
towords 6 = "six"
towords 7 = "seven"
towords 8 = "eight"
towords 9 = "nine"
towords 10 = "ten"

ordinalWords :: Integral i => i -> Text
ordinalWords 1 = "first"
ordinalWords 2 = "second"
ordinalWords 3 = "third"
ordinalWords 4 = "fourth"
ordinalWords 5 = "fifth"
