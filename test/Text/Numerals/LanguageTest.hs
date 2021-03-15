module Text.Numerals.LanguageTest (
    testLanguage, testTimeLanguage
  ) where

import Data.Int(Int8, Int16, Int32, Int64)
import Data.Text(Text)
import Data.Word(Word8, Word16, Word32, Word64)

import Test.Hspec(SpecWith, describe, it, shouldBe)
import Test.QuickCheck(property)

import Text.Numerals.Class(toCardinal, toOrdinal, toShortOrdinal, toTimeText')
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

testTimeConversion :: (Int -> Int -> Text) -> Int -> Int -> Text -> SpecWith ()
testTimeConversion f h m t = it (show (h, m)) (f h m `shouldBe` t)

testEquivalenceCardinal :: Integral i => NumeralsAlgorithm -> i -> Bool
testEquivalenceCardinal al i = toCardinal al i == toCardinal al (fromIntegral i :: Integer)

testEquivalenceOrdinal :: Integral i => NumeralsAlgorithm -> i -> Bool
testEquivalenceOrdinal al i = toOrdinal al i == toOrdinal al (fromIntegral i :: Integer)

testEquivalenceShortOrdinal :: Integral i => NumeralsAlgorithm -> i -> Bool
testEquivalenceShortOrdinal al i = toShortOrdinal al i == toShortOrdinal al (fromIntegral i :: Integer)


testLanguage :: String -> NumeralsAlgorithm -> [(Integer, Text)] -> [(Integer, Text)] -> [(Integer, Text)] -> SpecWith ()
testLanguage languageName al cs os ss = describe languageName $ do
    describe "Automatied tests" $ do
        it "Different cardinal names" (property (testDifferCardinal al))
        it "Different ordinal names" (property (testDifferOrdinal al))
        it "Difference between cardinal and ordinal names" (property (testDifferCardinalOrdinal al))
        it "Difference between cardinal and ordinal names with different number" (property (testDifferCardinalOrdinal' al))
        it "Check toCardinal algorithm with Int" (property (testEquivalenceCardinal al :: Int -> Bool))
        it "Check toCardinal algorithm with Int8" (property (testEquivalenceCardinal al :: Int8 -> Bool))
        it "Check toCardinal algorithm with Int16" (property (testEquivalenceCardinal al :: Int16 -> Bool))
        it "Check toCardinal algorithm with Int32" (property (testEquivalenceCardinal al :: Int32 -> Bool))
        it "Check toCardinal algorithm with Int64" (property (testEquivalenceCardinal al :: Int64 -> Bool))
        it "Check toCardinal algorithm with Word" (property (testEquivalenceCardinal al :: Word -> Bool))
        it "Check toCardinal algorithm with Word8" (property (testEquivalenceCardinal al :: Word8 -> Bool))
        it "Check toCardinal algorithm with Word16" (property (testEquivalenceCardinal al :: Word16 -> Bool))
        it "Check toCardinal algorithm with Word32" (property (testEquivalenceCardinal al :: Word32 -> Bool))
        it "Check toCardinal algorithm with Word64" (property (testEquivalenceCardinal al :: Word64 -> Bool))
        it "Check toOrdinal algorithm with Int" (property (testEquivalenceOrdinal al :: Int -> Bool))
        it "Check toOrdinal algorithm with Int8" (property (testEquivalenceOrdinal al :: Int8 -> Bool))
        it "Check toOrdinal algorithm with Int16" (property (testEquivalenceOrdinal al :: Int16 -> Bool))
        it "Check toOrdinal algorithm with Int32" (property (testEquivalenceOrdinal al :: Int32 -> Bool))
        it "Check toOrdinal algorithm with Int64" (property (testEquivalenceOrdinal al :: Int64 -> Bool))
        it "Check toOrdinal algorithm with Word" (property (testEquivalenceOrdinal al :: Word -> Bool))
        it "Check toOrdinal algorithm with Word8" (property (testEquivalenceOrdinal al :: Word8 -> Bool))
        it "Check toOrdinal algorithm with Word16" (property (testEquivalenceOrdinal al :: Word16 -> Bool))
        it "Check toOrdinal algorithm with Word32" (property (testEquivalenceOrdinal al :: Word32 -> Bool))
        it "Check toOrdinal algorithm with Word64" (property (testEquivalenceOrdinal al :: Word64 -> Bool))
        it "Check toShortOrdinal algorithm with Int" (property (testEquivalenceShortOrdinal al :: Int -> Bool))
        it "Check toShortOrdinal algorithm with Int8" (property (testEquivalenceShortOrdinal al :: Int8 -> Bool))
        it "Check toShortOrdinal algorithm with Int16" (property (testEquivalenceShortOrdinal al :: Int16 -> Bool))
        it "Check toShortOrdinal algorithm with Int32" (property (testEquivalenceShortOrdinal al :: Int32 -> Bool))
        it "Check toShortOrdinal algorithm with Int64" (property (testEquivalenceShortOrdinal al :: Int64 -> Bool))
        it "Check toShortOrdinal algorithm with Word" (property (testEquivalenceShortOrdinal al :: Word -> Bool))
        it "Check toShortOrdinal algorithm with Word8" (property (testEquivalenceShortOrdinal al :: Word8 -> Bool))
        it "Check toShortOrdinal algorithm with Word16" (property (testEquivalenceShortOrdinal al :: Word16 -> Bool))
        it "Check toShortOrdinal algorithm with Word32" (property (testEquivalenceShortOrdinal al :: Word32 -> Bool))
        it "Check toShortOrdinal algorithm with Word64" (property (testEquivalenceShortOrdinal al :: Word64 -> Bool))

    describe "Test cardinal numbers" (mapM_ (uncurry (testNumberConversion (toCardinal al))) cs)
    describe "Test ordinal numbers" (mapM_ (uncurry (testNumberConversion (toOrdinal al))) os)
    describe "Test short ordinal numbers" (mapM_ (uncurry (testNumberConversion (toShortOrdinal al))) ss)

testTimeLanguage :: String -> NumeralsAlgorithm -> [(Int, Int, Text)] -> SpecWith ()
testTimeLanguage languageName al ts = describe languageName (describe "test time to text" (mapM_ (\(h, m, t) -> testTimeConversion (toTimeText' al) h m t) ts))
