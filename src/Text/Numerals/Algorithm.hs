{-# LANGUAGE OverloadedStrings, Safe #-}

module Text.Numerals.Algorithm where

import Data.Text(Text)

data NumeralsAlgorithm = NumeralsAlgorithm {
    minusWord :: Text
--  , numberVal :: Intagral i
  }

-- splitNum :: Integral i -> (Text, i)
-- splitNum 

toCardinal :: Integral i => NumeralsAlgorithm -> i -> Text
toCardinal NumeralsAlgorithm { minusWord=minusWord } = cardinal
    where cardinal i
              | i < 0 = minusWord <> go i
              | otherwise = go i
              where go _ = ""
