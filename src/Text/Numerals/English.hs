{-# LANGUAGE OverloadedStrings, Safe #-}

module Text.Numerals.English where

import Data.Text(Text)

import Text.Numerals.Internal(_div10, _rem10, _showText)

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
