{-# LANGUAGE Safe #-}

module Text.Numerals.Class where

import Data.Text(Text)

data NumberType
  = Cardinal
  | Ordinal
  deriving (Bounded, Enum, Eq, Ord)

-- | A type class used for num to word algorithms. It maps an 'Integral' type i
-- to 'Text'.
class NumToWord a where
    toCardinal :: Integral i => a -> i -> Text
    toOrdinal :: Integral i => a -> i -> Text
    toNumber :: Integral i => NumberType -> a -> i -> Text
    toNumber Cardinal = toCardinal
    toNumber Ordinal = toOrdinal

class ValueSplit a where
    valueSplit :: Integral i => a -> i -> Maybe (i, Text)
