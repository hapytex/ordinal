{-# LANGUAGE OverloadedLists, OverloadedStrings, TemplateHaskell #-}

{-|
Module      : Text.Numerals.Languages.Dutch
Description : A module to convert numbers to words in the /Dutch/ language.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module contains logic to convert numbers to words in the /Dutch/ language.
-}

module Text.Numerals.Languages.Dutch (
    -- * Num to word algorithm
    dutch
    -- * Convert a cardinal number to text
  , toCardinal'
    -- * Convert to ordinal
  , ordinize'
    -- * Constant words
  , negativeWord', zeroWord', oneWord'
    -- * Names for numbers
  , lowWords', midWords', highWords'
    -- * Merge function
  , merge'
  ) where

import Data.Text(Text, pack, snoc)
import Data.Vector(Vector)

import Text.Numerals.Algorithm(HighNumberAlgorithm(LongScale), NumeralsAlgorithm, numeralsAlgorithm)
import Text.Numerals.Algorithm.Template(ordinizeFromDict)
import Text.Numerals.Class(valueSplit, toCardinal)
import Text.Numerals.Internal(_million, _mergeWithSpace, _showIntegral)

$(pure (ordinizeFromDict "_ordinize'" [
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
  ] 'id))

-- | A function that converts a number in words in /cardinal/ form to /ordinal/
-- form according to the /Dutch/ language rules.
ordinize' :: Text -> Text
ordinize' = (`snoc` 'e') . _ordinize'

-- | A 'NumeralsAlgorithm' to convert numbers to words in the /Dutch/ language.
dutch :: NumeralsAlgorithm  -- ^ A 'NumeralsAlgorithm' that can be used to convert numbers to different formats.
dutch = numeralsAlgorithm negativeWord' zeroWord' oneWord' lowWords' midWords' (valueSplit highWords') merge' ordinize' shortOrdinal'

-- | Convert numers to their cardinal counterpart in /Dutch/.
toCardinal' :: Integral i
  => i  -- ^ The number to convert to text.
  -> Text  -- ^ The cardinal counterpart in /Dutch/.
toCardinal' = toCardinal dutch

-- | The words used to mark a negative number in the /Dutch/ language.
negativeWord' :: Text
negativeWord' = "min"

-- | The word used for the number /zero/ in the /Dutch/ language.
zeroWord' :: Text
zeroWord' = "nul"

-- | The word used for the number /one/ in the /Dutch/ language.
oneWord' :: Text
oneWord' = "één"

-- | A 'Vector' that contains the word used for the numbers /two/ to /twenty/ in the /Dutch/ language.
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
  , "achttien"
  , "negentien"
  , "twintig"
  ]

-- | A list of 2-tuples that contains the names of values between /thirty/ and
-- /thousand/ in the /Dutch/ language.
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

-- | A merge function that is used to combine the names of words together to
-- larger words, according to the /Dutch/ grammar rules.
merge' :: Integral i => i -> i -> Text -> Text -> Text
merge' 1 r
    | r < _million = const id
    | otherwise = const (_merge' 1 r "een")
merge' l r = _merge' l r

_merge' :: Integral i => i -> i -> Text -> Text -> Text
_merge' l r
    | r > l && r >= _million = _mergeWithSpace
    | r > l = (<>)
    | r < 10 && 10 < l && l < 100 = go
    | l >= _million = _mergeWithSpace
    | otherwise = (<>)
    where go tl tr = _leftAnd r tr <> _rightAnd l tl

-- | An algorithm to obtain the names of /large/ numbers (one million or larger)
-- in /Dutch/. Dutch uses a /long scale/ with the @iljoen@ and @iljard@
-- suffixes.
highWords' :: HighNumberAlgorithm
highWords' = LongScale "iljoen" "iljard"

-- | A function to convert a number to its /short ordinal/ form in /Dutch/.
shortOrdinal' :: Integral i
  => i  -- ^ The number to convert to /short ordinal/ form.
  -> Text  -- ^ The equivalent 'Text' specifying the number in /short ordinal/ form.
shortOrdinal' = pack . (`_showIntegral` "e")


