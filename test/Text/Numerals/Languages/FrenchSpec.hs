module Text.Numerals.Languages.FrenchSpec where

import Data.Text(Text)

import Test.Hspec(Spec)
import Text.Numerals.Languages.French(french)
import Text.Numerals.LanguageTest(testLanguage)

spec :: Spec
spec = testLanguage "French" french cardinals ordinals

cardinals :: [(Int, Text)]
cardinals = [
  ]

ordinals :: [(Int, Text)]
ordinals = [
  ]
