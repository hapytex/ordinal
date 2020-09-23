{-# LANGUAGE Safe #-}

module Text.Numerals.Class where

import Data.Text(Text)

class NumToWord a where
    toCardinal :: Integral i => a -> i -> Text
    toOrdinal :: Integral i => a -> i -> Text

class ValueSplit a where
    valueSplit :: Integral i => a -> i -> Maybe (i, Text)
