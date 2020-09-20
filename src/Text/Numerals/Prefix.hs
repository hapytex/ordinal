{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Text.Numerals.Prefix where

import Data.Text(Text)
import Data.Vector(Vector, (!?))

greekPrefixes :: Vector Text
greekPrefixes = [
    "m"
  , "b"
  , "tr"
  , "quadr"
  , "quint"
  , "sext"
  , "oct"
  , "non"
  , "dec"
  , "undec"
  , "duodec"
  , "tredec"
  , "quattuordec"
  , "quindec"
  , "sexdec"
  , "septended"
  , "octodec"
  , "novemdec"
  ]

greekPrefix :: Integral i => i -> Maybe Text
greekPrefix n = greekPrefixes !? (fromIntegral n - 1)
