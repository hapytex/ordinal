module Text.Numerals.Languages.DutchSpec where

import Data.Text(Text)

import Test.Hspec(Spec)
import Text.Numerals.Languages.Dutch(dutch)
import Text.Numerals.LanguageTest(testLanguage)

spec :: Spec
spec = testLanguage "Dutch" dutch cardinals ordinals

cardinals :: [(Int, Text)]
cardinals = [
  ]

ordinals :: [(Int, Text)]
ordinals = [
  ]
