{-# LANGUAGE Safe #-}

module Text.Numerals.Class where

import Data.Text(Text)

class NumToWord a where
    toCardinal :: Integral i => a -> i -> Text
    toOrdinal :: Integral i => a -> i -> Text
