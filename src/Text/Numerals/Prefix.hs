{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Text.Numerals.Prefix where

import Data.Text(Text)
import Data.Vector(Vector, (!?), fromList)

greekPrefixes' :: [Text]
greekPrefixes' = [
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

greekPrefixes :: Vector Text
greekPrefixes = fromList greekPrefixes'

greekPrefix :: Integral i => i -> Maybe Text
greekPrefix n = greekPrefixes !? (fromIntegral n - 1)
