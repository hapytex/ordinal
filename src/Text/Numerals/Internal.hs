{-# LANGUAGE Safe #-}

module Text.Numerals.Internal (
    _div10, _rem10
  , _showText
  ) where

import Data.Text(Text, pack)

_showText :: Show a => a -> Text
_showText = pack . show

_div10 :: Integral i => i -> i
_div10 = (`div` 10)

_rem10 :: Integral i => i -> i
_rem10 = (`rem` 10)

