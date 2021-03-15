{-# LANGUAGE OverloadedStrings #-}

module Text.Numerals.Languages.KlingonSpec (
    spec
  ) where

import Data.Text(Text)

import Test.Hspec(Spec)
import Text.Numerals.Languages.Klingon(klingon)
import Text.Numerals.LanguageTest(testLanguage, testTimeLanguage)

spec :: Spec
spec = testLanguage "Klingon" klingon cardinals ordinals shortOrdinals >> testTimeLanguage "klingon" klingon timeText

cardinals :: [(Integer, Text)]
cardinals = [
    (1, "???")
  , (2, "???")
  ]

ordinals :: [(Integer, Text)]
ordinals = [
    (1, "???")
  , (2, "???")
  ]

shortOrdinals :: [(Integer, Text)]
shortOrdinals = [
    (1, "???")
  , (2, "???")
  ]

timeText :: [(Int, Int, Text)]
timeText = [
    (0, 0, "pagh rep")
  , (0, 25, "pagh cha’maH vagh rep")
  , (0, 50, "pagh vagh maH rep")
  , (1, 0, "wa’vatlh rep")
  , (1, 5, "wa’vatlh vagh rep")
  , (1, 10, "wa’vatlh wa’maH rep")
  , (6, 0, "jav vatlh rep")
  , (10, 0, "wa’maH vatlh rep")
  , (11, 0, "wa’maH wa’vatlh rep")
  , (12, 0, "wa’maH cha’vatlh rep")
  , (13, 0, "wa’maH wej vatlh rep")
  , (14, 30, "wa’maH loS vatlh wej maH rep")
  , (19, 0, "wa’maH Hut vatlh rep")
  , (23, 0, "cha’maH wej vatlh rep")
  ]
