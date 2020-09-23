{-# LANGUAGE OverloadedLists, OverloadedStrings, TemplateHaskell #-}

module Text.Numerals.Languages.Dutch where

import Data.Text(Text, isSuffixOf, snoc)
import Data.Vector(Vector)

import Text.Numerals.Algorithm(NumeralsAlgorithm, generatePrefixedHighNumbers, numeralsAlgorithm, ordinizeFromDict)
import Text.Numerals.Internal(_million, _mergeWith, _mergeWithSpace, _mergeWith')

-- TODO: add "e" at the end
$(pure [ordinizeFromDict "_ordinize'" [
    ("nul", "nuld")
  , ("één", "eerst")
  , ("twee", "tweed")
  , ("drie", "derd")
  , ("vier", "vierd")
  , ("vijf", "vijfd")
  , ("zes", "zesd")
  , ("zeven", "zevend")
  , ("acht", "achtst")
  , ("negen", "negend")
  , ("tien", "tiend")
  , ("elf", "elfd")
  , ("twaalf", "twaalfd")
  , ("ig", "igst")
  , ("erd", "erdst")
  , ("end", "endst")
  , ("joen", "joenst")
  , ("rd", "rdst")
  ]])

ordinize' :: Text -> Text
ordinize' = (`snoc` 'e') . _ordinize'

dutch :: NumeralsAlgorithm
dutch = numeralsAlgorithm negativeWord' zeroWord' oneWord' lowWords' midWords' highWords' merge' ordinize'

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

midWords' :: [(Integer, Text)]
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

_rightAnd :: Integral i => i -> Text -> Text
_rightAnd 1 = const "een"
_rightAnd _ = id

_leftAnd :: Integral i => i -> Text -> Text
_leftAnd 1 = const "eenen"
_leftAnd n | 2 <- n = addE
           | 3 <- n = addE
           | otherwise = (<> "en")
           where addE = (<> "ën")

merge' :: Integral i => i -> i -> Text -> Text -> Text
merge' 1 r | r < _million = const id
merge' l r | r > l && r > _million = _mergeWithSpace
           | r > l = (<>)
           | r < 10 && 10 < l && l < 100 = go
           | l > _million = _mergeWithSpace
           | otherwise = (<>)
    where go tl tr = _leftAnd r tr <> _rightAnd l tl

highWords' :: [(Integer, Text)]
highWords' = generatePrefixedHighNumbers ["iljoen", "iljard"]
