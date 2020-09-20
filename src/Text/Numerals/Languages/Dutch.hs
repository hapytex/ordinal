{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Text.Numerals.Languages.Dutch where

import Data.Text(Text, isSuffixOf, snoc)
import Data.Vector(Vector)

import Text.Numerals.Algorithm(NumeralsAlgorithm, numeralsAlgorithm)
import Text.Numerals.Internal(_million, _mergeWith, _mergeWith')

dutch :: NumeralsAlgorithm
dutch = numeralsAlgorithm negativeWord' zeroWord' oneWord' lowWords' midWords' merge'

negativeWord' :: Text
negativeWord' = "min"

zeroWord' :: Text
zeroWord' = "nul"

oneWord' :: Text
oneWord' = "één"

lowWords' :: Vector Text
lowWords' = [
    "twee"
  , "drie"
  , "vier"
  , "vijf"
  , "zes"
  , "zeven"
  , "acht"
  , "negen"
  , "tien"
  , "elf"
  , "twaalf"
  , "dertien"
  , "veertien"
  , "vijftien"
  , "zestien"
  , "zeventien"
  , "achtien"
  , "negentien"
  , "twintig"
  ]

midWords' :: [(Int, Text)]
midWords' = [
    (1000, "duizend")
  , (100, "honderd")
  , (90, "negentig")
  , (80, "tachtig")
  , (70, "zeventig")
  , (60, "zestig")
  , (50, "vijftig")
  , (40, "veertig")
  , (30, "dertig")
  ]

_one :: Integral i => i -> Text -> Text
_one 1 = const "een"
_one _ = id

_one' :: Integral i => i -> Text -> Text
_one' 1 _ = "eenen"
_one' _ t | isSuffixOf "e" t = t <> "ën"
          | otherwise = t <> "en"

merge' :: Integral i => i -> i -> Text -> Text -> Text
merge' 1 r | r < _million = const id
merge' l r | r > l && r > _million = _mergeWith' ' '
           | r > l = (<>)
           | r < 10 && 10 < l && l < 100 = go
           | l > _million = _mergeWith' ' '
           | otherwise = (<>)
    where go tl tr = _one' r tr <> _one l tl
