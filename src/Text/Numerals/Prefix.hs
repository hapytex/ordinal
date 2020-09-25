{-# LANGUAGE OverloadedLists, OverloadedStrings #-}

module Text.Numerals.Prefix (
    latinPrefixes, latinPrefixes', latinPrefix
  ) where

import Data.Text(Text)
import Data.Vector(Vector, (!?), fromList)

latinPrefixes' :: [Text]
latinPrefixes' = [
    "m"
  , "b"
  , "tr"
  , "quadr"
  , "quint"
  , "sext"
  , "sept"
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
  , "vigint"
  ]

latinPrefixes :: Vector Text
latinPrefixes = fromList latinPrefixes'

latinPrefix :: Integral i => i -> Maybe Text
latinPrefix n = latinPrefixes !? (fromIntegral n - 1)
