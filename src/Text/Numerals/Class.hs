{-# LANGUAGE RankNTypes, Safe #-}

{-|
Module      : Text.Numerals.Class
Description : A module that contains the typeclasses on which the rest of the module works.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines the typeclasses that are used in the rest of the module. The 'NumToWord' class
is the typeclass that is used by all algorithmic conversion tools.
-}

module Text.Numerals.Class (
    -- * Typeclasses
    NumToWord(toCardinal, toOrdinal, toWords)
  , ValueSplit(valueSplit)
    -- * Segmenting a number
  , NumberSegment(NumberSegment, segmentDivision, segmentValue, segmentText, segmentRemainder)
  , MNumberSegment
    -- * Utility type synonyms
  , MergerFunction, FreeMergerFunction, ValueSplitter, FreeValueSplitter, NumberSegmenting
  ) where

import Data.Text(Text)

-- | A type alias of a function that is used to merge the names of two numbers according
-- to gramatical rules. The type parameter is the type of the numbers to merge.
type MergerFunction i = i -> i -> Text -> Text -> Text

-- | A type alias of a 'MergeFunction' function with a free 'Integral' variable.
type FreeMergerFunction = forall i . Integral i => MergerFunction i

-- | A type alias of a function that maps a number to a 2-tuple that contains a
-- number and the word for that number. This number is normally the largest
-- number smaller than the given number. In case no name exists for a number
-- smaller than the given one 'Nothing' is returned.
type ValueSplitter i = i -> Maybe (i, Text)

-- | A type alias of a 'ValueSplitter' function, with a free 'Integral'
-- variable.
type FreeValueSplitter = forall i . Integral i => ValueSplitter i

-- | A type alias of a function that converts a number to a 'NumberSegment' for that number.
type NumberSegmenting i = i -> NumberSegment i

-- | A data type used to convert a number into segments. Each segment has an
-- optional division and remainder part, together with a value and the name of
-- that value in a language.
data NumberSegment i = NumberSegment {
    segmentDivision :: MNumberSegment i  -- ^ The optional division part. 'Nothing' if the division is equal to one.
  , segmentValue :: i  -- ^ The value of the given segment.
  , segmentText :: Text  -- ^ The name of the value of the given segment, in a specific language.
  , segmentRemainder ::  MNumberSegment i  -- ^ The optional remainder part. 'Nothing' if the remainder is equal to zero.
  } deriving (Eq, Ord, Read, Show)

-- | A 'Maybe' variant of the 'NumberSegment' data type. This is used since the
-- division part can be one, or the remainder part can be zero.
type MNumberSegment i = Maybe (NumberSegment i)

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
    toCardinal = toWords Cardinal
    toOrdinal :: Integral i
      => a  -- ^ The conversion algorithm that transforms the number into words.
      -> i  -- ^ The number to transform into a /ordinal/ form.
      -> Text  -- ^ The number in words in a /ordinal/ form.
    toOrdinal = toWords Ordinal
    toWords :: Integral i
      => NumberType  -- ^ The given format to convert the number to.
      -> a  -- ^ The conversion algorithm that transforms the number into words.
      -> i  -- ^ The number to transform into the given form.
      -> Text  -- ^ The number in words in the given form.
    toWords Cardinal = toCardinal
    toWords Ordinal = toOrdinal
    {-# MINIMAL toCardinal, toOrdinal | toWords #-}

-- | A type class used to split a value, based on the name of a number in a
-- specific language. The value that is used to split, is often, depending on
-- the language, the largest value smaller than the given number.
class ValueSplit a where
    -- | A function that takes an 'Integral' value, and based on the object
    -- splits it with a value and the name of the number in a specific language.
    valueSplit :: a -> FreeValueSplitter
