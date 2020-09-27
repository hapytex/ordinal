module Text.Numerals.Languages.EnglishSpec where

import Data.Text(Text)

import Test.Hspec(Spec)
import Text.Numerals.Languages.English(english)
import Text.Numerals.LanguageTest(testLanguage)

spec :: Spec
spec = testLanguage "English" english cardinals ordinals

cardinals :: [(Int, Text)]
cardinals = [
  ]

ordinals :: [(Int, Text)]
ordinals = [
  ]
