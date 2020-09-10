{-# LANGUAGE OverloadedStrings, Safe #-}

module Text.Numerals.Prefix where

import Data.Text(Text)

greekPrefix :: Integral i => i -> Text
greekPrefix 1 = "m"
greekPrefix 2 = "b"
greekPrefix 3 = "tr"
greekPrefix 4 = "quadr"
greekPrefix 5 = "quint"
greekPrefix 6 = "sext"
greekPrefix 7 = "sept"
greekPrefix 8 = "oct"
greekPrefix 9 = "non"
greekPrefix 10 = "dec"
greekPrefix 11 = "undec"
greekPrefix 12 = "duodec"
greekPrefix 13 = "tredec"
greekPrefix 14 = "quattuordec"
greekPrefix 15 = "quindec"
greekPrefix 16 = "sexdec"
greekPrefix 17 = "septendec"
greekPrefix 18 = "octodec"
greekPrefix 19 = "novemdec"
