{-# LANGUAGE NumericUnderscores, Safe #-}

module Text.Numerals.Internal (
    _div10, _rem10
  , _showText
  , _mergeWith, _mergeWith'
  , _thousand, _million
  ) where

import Data.Text(Text, cons, pack)

_mergeWith' :: Char -> Text -> Text -> Text
_mergeWith' m = (. cons m) . (<>)

_mergeWith :: Text -> Text -> Text -> Text
_mergeWith m = (<>) . (<> m)

_showText :: Show a => a -> Text
_showText = pack . show

_div10 :: Integral i => i -> i
_div10 = (`div` 10)

_rem10 :: Integral i => i -> i
_rem10 = (`rem` 10)

_thousand :: Integral i => i
_thousand = 1000

_million :: Integral i => i
_million = 1_000_000

_billion :: Integral i => i
_billion = 1_000_000_000
