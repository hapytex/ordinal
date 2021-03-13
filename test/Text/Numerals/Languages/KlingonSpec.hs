module Text.Numerals.Languages.KlingonSpec (
    spec
  ) where

import Data.Text(Text)

import Test.Hspec(Spec)
import Text.Numerals.Languages.Klingon(Klingon)
import Text.Numerals.LanguageTest(testLanguage)

spec :: Spec
spec = testLanguage "Klingon" Klingon cardinals ordinals shortOrdinals

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
