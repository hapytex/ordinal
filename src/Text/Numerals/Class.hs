{-# LANGUAGE DeriveFunctor, DeriveFoldable, RankNTypes, Safe #-}

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
    NumToWord(toCardinal, toOrdinal, toShortOrdinal, toWords, toTimeText, toTimeText')
  , ValueSplit(valueSplit)
    -- * Types of numbers
  , NumberType(Cardinal, Ordinal, ShortOrdinal)
    -- * Segmenting a number
  , NumberSegment(NumberSegment, segmentDivision, segmentValue, segmentText, segmentRemainder)
  , MNumberSegment
    -- * Segments of time
  , ClockSegment(OClock, Past, QuarterPast, ToHalf, Half, PastHalf, QuarterTo, To)
  , DayPart(Night, Morning, Afternoon, Evening)
  , DaySegment(DaySegment, dayPart, dayHour)
  , toDayPart, toDaySegment, toClockSegment
  , hourCorrection
    -- * Convert the current time to words
  , currentTimeText, currentTimeText'
    -- * Utility type synonyms
  , NumberToWords,  FreeNumberToWords
  , MergerFunction, FreeMergerFunction, ValueSplitter, FreeValueSplitter, NumberSegmenting
  , ClockText
  ) where

import Data.Default(Default(def))
import Data.Text(Text)
import Data.Time.Clock(getCurrentTime, utctDayTime)
import Data.Time.LocalTime(TimeOfDay(TimeOfDay), TimeZone, timeToTimeOfDay, utcToLocalTimeOfDay)

-- | A type alias for a function that maps a number to a 'Text' object.
type NumberToWords i = i -> Text

-- | A type alias for a 'NumberToWords' function, with a free 'Integral'
-- variable.
type FreeNumberToWords = forall i . Integral i => NumberToWords i

-- | A type alias of a function that is used to merge the names of two numbers according
-- to gramatical rules. The type parameter is the type of the numbers to merge.
type MergerFunction i = i -> i -> Text -> Text -> Text

-- | A type alias of a 'MergerFunction' function with a free 'Integral' variable.
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
  } deriving (Foldable, Functor, Eq, Ord, Read, Show)

-- | A 'Maybe' variant of the 'NumberSegment' data type. This is used since the
-- division part can be one, or the remainder part can be zero.
type MNumberSegment i = Maybe (NumberSegment i)

-- | A data type that specifies the different types of numbers. These can be
-- used to specify the "target format". The 'Default' number type is 'Cardinal'.
data NumberType
  = Cardinal  -- ^ /Cardinal/ numbers like one, two, three, etc.
  | Ordinal  -- ^ /Ordinal/ numbers like first, second, third, etc.
  | ShortOrdinal -- ^ /Short ordinal/ numbers like 1st, 2nd, 3rd, etc.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | The type of a function that converts time to its description. The first
-- two parameters are used to make conversion more convenient.
type ClockText
  =  ClockSegment  -- ^ The 'ClockSegment' that describes the state of minutes within an hour.
  -> DaySegment  -- ^ The 'DaySegment' that describes the state of hours within a day.
  -> Int  -- ^ The number of hours.
  -> Int  -- ^ The number of minutes.
  -> Text  -- ^ A 'Text' object that describes the given time.

-- | A data type that describes the state of the minutes within an hour.
data ClockSegment
  = OClock  -- ^ The number of minutes is zero.
  | Past Int  -- ^ The parameter is the number of minutes past the hour, this is between @1@ and @14@.
  | QuarterPast  -- ^ It is a quarter past the hour.
  | ToHalf Int  -- ^ The parameter is the number of minutes until half, this is between @1@ and @14@.
  | Half  -- ^ It is half past an hour.
  | PastHalf Int  -- ^ The parameter is the number of minutes past half, this is between @1@ and @14@.
  | QuarterTo  -- ^ It is a quarter to an hour.
  | To Int  -- ^ The parameter is the number of minutes to the next hour, this is between @1@ and @14@.
  deriving (Eq, Ord, Read, Show)

-- | A data type that describes the state of the hours within a day.
data DayPart
  = Night  -- ^ It is night, this means that it is between @0:00@ and @5:59@.
  | Morning  -- ^ It is morning, this means that it is between @6:00@ and @11:59@.
  | Afternoon  -- ^ It is afternoon, this means it is between @12:00@ and @17:59@.
  | Evening  -- ^ It is evening, this means it is between @18:00@ and @23:59@.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | A data type that describes the part of the day, and the number of hours on
-- a 12-hour clock.
data DaySegment
  = DaySegment {
        dayPart :: DayPart  -- ^ The part of the day.
      , dayHour :: Int  -- ^ The number of hours, between @1@ and @12@ (both inclusive).
      }
  deriving (Eq, Ord, Read, Show)


-- | Convert the given number of minutes to the corresponding 'ClockSegment'.
toClockSegment
  :: Int  -- ^ The number of minutes.
  -> ClockSegment  -- ^ The corresponding 'ClockSegment'.
toClockSegment 0 = OClock
toClockSegment 15 = QuarterPast
toClockSegment 30 = Half
toClockSegment 45 = QuarterTo
toClockSegment n
    | n <= 15 = Past n
    | n <= 30 = ToHalf (30-n)
    | n <= 45 = PastHalf (n-30)
    | otherwise = To (60-n)

-- | Convert the given number of hours to the corresponding 'DayPart'.
toDayPart
  :: Int  -- ^ The given number of hours.
  -> DayPart  -- ^ The corresponding 'DayPart'.
toDayPart n
    | n <= 5 = Night
    | n <= 11 = Morning
    | n <= 17 = Afternoon
    | otherwise = Evening

-- | Convert the given number of hours to the corresponding 'DaySegment'.
toDaySegment
  :: Int  -- ^ The given number of hours.
  -> DaySegment  -- ^ The corresponding 'DaySegment'.
toDaySegment n = DaySegment (toDayPart n) (hourCorrection n)

-- | Correct the hour to a 12 number segment.
-- The input can be any Int number, whereas the
-- result will be in the @1 .. 12@ range.
hourCorrection
  :: Int  -- ^ The value for the number of hours.
  -> Int  -- ^ The hours in the @1 .. 12@ range.
hourCorrection h = ((h - 1) `mod` 12) + 1

instance Default NumberType where
    def = Cardinal

-- | A type class used for num to word algorithms. It maps an 'Integral' type
-- @i@ to 'Text'.
class NumToWord a where
    -- | Convert the given number to a 'Text' object that is the given number in
    -- words in /cardinal/ form.
    toCardinal :: Integral i
      => a  -- ^ The conversion algorithm that transforms the number into words.
      -> i  -- ^ The number to transform into a /cardinal/ form.
      -> Text  -- ^ The number in words in a /cardinal/ form.
    toCardinal = toWords Cardinal

    -- | Convert the given number to a 'Text' object that is the given number in
    -- words in /cardinal/ form.
    toOrdinal :: Integral i
      => a  -- ^ The conversion algorithm that transforms the number into words.
      -> i  -- ^ The number to transform into a /ordinal/ form.
      -> Text  -- ^ The number in words in a /ordinal/ form.
    toOrdinal = toWords Ordinal

    -- | Convert the given number to a 'Text' object that is the given number
    -- in words in /short cardinal/ form.
    toShortOrdinal :: Integral i
      => a  -- ^ The conversion algorithm that transforms the number into words.
      -> i  -- ^ The number to transform into a /ordinal/ form.
      -> Text  -- ^ The number in words in a /ordinal/ form.
    toShortOrdinal = toWords Ordinal

    -- | Convert the given number to a 'Text' object that is the given number in
    -- words in the given 'NumberType'.
    toWords :: Integral i
      => NumberType  -- ^ The given format to convert the number to.
      -> a  -- ^ The conversion algorithm that transforms the number into words.
      -> i  -- ^ The number to transform into the given form.
      -> Text  -- ^ The number in words in the given form.
    toWords Cardinal = toCardinal
    toWords Ordinal = toOrdinal
    toWords ShortOrdinal = toShortOrdinal
    
    -- | Convert the given time of the day to text describing that time.
    toTimeText
      :: a  -- ^ The conversion algorithm to transform numbers into words.
      -> TimeOfDay  -- ^ The time of the day to convert to words.
      -> Text  -- ^ The time as /text/.
    toTimeText gen (TimeOfDay h m _) = toTimeText' gen h m
    
    -- | Convert the given hours and minutes to text that describes the time.
    toTimeText'
      :: a  -- ^ The conversion algorithm to transform numbers into words.
      -> Int  -- ^ The number of hours, between 0 and 23 (both inclusive)
      -> Int  -- ^ The number of minutes, beween 0 and 59 (both inclusive)
      -> Text  -- ^ The time as /text/.
    toTimeText' gen h m = toTimeText gen (TimeOfDay h m 0)
    {-# MINIMAL ((toCardinal, toOrdinal, toShortOrdinal) | toWords), (toTimeText | toTimeText') #-}

-- | Convert the current time in the given 'TimeZone' to the time in words with the given 'NumToWord'
-- algorithm.
currentTimeText :: NumToWord a
  => TimeZone -- ^ The given 'TimeZone'.
  -> a  -- ^ The 'NumToWord' algorithm that converts time to words.
  -> IO Text  -- ^ An 'IO' that will generate a 'Text' object that describes the current time in words.
currentTimeText tz alg = toTimeText alg . snd . utcToLocalTimeOfDay tz . timeToTimeOfDay . utctDayTime <$> getCurrentTime

-- | Convert the current time to the time in words with the given 'NumToWord'
-- algorithm as UTC time.
currentTimeText' :: NumToWord a
  => a  -- ^ The 'NumToWord' algorithm that converts time to words.
  -> IO Text  -- ^ An 'IO' that will generate a 'Text' object that describes the current time in words.
currentTimeText' alg = toTimeText alg . timeToTimeOfDay . utctDayTime <$> getCurrentTime

-- | A type class used to split a value, based on the name of a number in a
-- specific language. The value that is used to split, is often, depending on
-- the language, the largest value smaller than the given number.
class ValueSplit a where
    -- | A function that takes an 'Integral' value, and based on the object
    -- splits it with a value and the name of the number in a specific language.
    valueSplit :: a -> FreeValueSplitter
