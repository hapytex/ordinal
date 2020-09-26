{-# LANGUAGE Safe #-}

{-|
Module      : Text.Numerals.Class
Description : A module that contains the typeclasses on which the rest of the module works.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines the typeclasses that are used in the rest of the module. The 'NumToWord' class
is the typeclass that is used by all algorithmic conversion tools.
-}

module Text.Numerals.Class where

import Data.Text(Text)

-- | A data type that specifies the different types of numbers. These can be
-- used to specify the "target format".
data NumberType
  = Cardinal  -- ^ /Cardinal/ numbers like one, two, three, etc.
  | Ordinal  -- ^ /Ordinal/ numbers like first, second, third, etc.
  deriving (Bounded, Enum, Eq, Ord)

-- | A type class used for num to word algorithms. It maps an 'Integral' type i
-- to 'Text'.
class NumToWord a where
    -- | Convert the given number to a 'Text' object that is the given number in
    -- words.
    toCardinal :: Integral i
      => a  -- ^ The conversion algorithm that transforms the number into words.
      -> i  -- ^ The number to transform into a /cardinal/ form.
      -> Text  -- ^ The number in words in a /cardinal/ form.
    toCardinal = toNumber Cardinal
    toOrdinal :: Integral i
      => a  -- ^ The conversion algorithm that transforms the number into words.
      -> i  -- ^ The number to transform into a /ordinal/ form.
      -> Text  -- ^ The number in words in a /ordinal/ form.
    toOrdinal = toNumber Ordinal
    toNumber :: Integral i
      => NumberType  -- ^ The given format to convert the number to.
      -> a  -- ^ The conversion algorithm that transforms the number into words.
      -> i  -- ^ The number to transform into the given form.
      -> Text  -- ^ The number in words in the given form.
    toNumber Cardinal = toCardinal
    toNumber Ordinal = toOrdinal
    {-# MINIMAL toNumber | toCardinal, toOrdinal #-}

-- | A type class used to split a value, based on the name of a number in a
-- specific language. The value that is used to split, is often, depending on
-- the language, the largest value smaller than the given number.
class ValueSplit a where
    -- | A function that takes an 'Integral' value, and based on the object
    -- splits it with a value and the name of the number in a specific language.
    valueSplit :: Integral i => a -> i -> Maybe (i, Text)
