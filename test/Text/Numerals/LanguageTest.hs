module Text.Numerals.LanguageTest where

import Data.Text(Text)

import Test.Hspec
import Test.QuickCheck

import Text.Numerals.Class(toCardinal, toOrdinal)
import Text.Numerals.Algorithm(NumeralsAlgorithm)

testDifferCardinal :: NumeralsAlgorithm -> Integer -> Integer -> Bool
testDifferCardinal al n1 n2 = n1 == n2 || toCardinal al n1 /= toCardinal al n2

testDifferOrdinal :: NumeralsAlgorithm -> Integer -> Integer -> Bool
testDifferOrdinal al n1 n2 = n1 == n2 || toOrdinal al n1 /= toOrdinal al n2

testDifferCardinalOrdinal :: NumeralsAlgorithm -> Integer -> Bool
testDifferCardinalOrdinal al n = toCardinal al n /= toOrdinal al n

testDifferCardinalOrdinal' :: NumeralsAlgorithm -> Integer -> Integer -> Bool
testDifferCardinalOrdinal' al n1 n2 = toCardinal al n1 /= toOrdinal al n2

testNumberConversion :: (Integer -> Text) -> Integer -> Text -> SpecWith ()
testNumberConversion f n t = it (show n) (f n `shouldBe` t)

testLanguage :: String -> NumeralsAlgorithm -> [(Integer, Text)] -> [(Integer, Text)] -> SpecWith ()
testLanguage languageName al cs os = describe languageName $ do
    describe "Automatied tests" $ do
        it "Different cardinal names" (property (testDifferCardinal al))
        it "Different ordinal names" (property (testDifferOrdinal al))
        it "Difference between cardinal and ordinal names" (property (testDifferCardinalOrdinal al))
        it "Difference between cardinal and ordinal names with different number" (property (testDifferCardinalOrdinal' al))
    describe "Test cardinal numbers" (mapM_ (uncurry (testNumberConversion (toCardinal al))) cs)
    describe "Test ordinal numbers" (mapM_ (uncurry (testNumberConversion (toOrdinal al))) os)
