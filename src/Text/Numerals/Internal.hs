{-# LANGUAGE CPP, Safe #-}

module Text.Numerals.Internal (
    _div10, _rem10, _divisableBy, _divisable100
  , _pluralize, _pluralize'
  , _showText
  , _mergeWith, _mergeWithSpace, _mergeWithHyphen, _mergeWith', _replaceSuffix
  , _hundred, _thousand, _million, _billion, _trillion
  , _iLog, _iLogFloor
  , _stripLastIf
  , _showIntegral
  , _showPositive
  , _genText, _shrinkText
  ) where

import Control.Applicative(liftA2)

import Data.Char(intToDigit)
#if __GLASGOW_HASKELL__ < 803
import Data.Semigroup((<>))
#endif
import Data.Text(Text, cons, dropEnd, inits, isSuffixOf, singleton, tails, pack)
import qualified Data.Text as T

import Test.QuickCheck(listOf)
import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary))
import Test.QuickCheck.Gen(Gen)

_pluralize :: a -> a -> Int -> a
_pluralize sing plur = go
    where go 1 = sing
          go (-1) = sing
          go _ = plur

_pluralize' :: a -> a -> Int -> a
_pluralize' sing plur = go
    where go 1 = sing
          go _ = plur

_stripLastIf :: Char -> Text -> Text
_stripLastIf c t
    | singleton c `isSuffixOf` t = T.init t
    | otherwise = t

_mergeWith' :: Char -> Text -> Text -> Text
_mergeWith' m = (. cons m) . (<>)

_mergeWithSpace :: Text -> Text -> Text
_mergeWithSpace = _mergeWith' ' '

_mergeWithHyphen :: Text -> Text -> Text
_mergeWithHyphen = _mergeWith' '-'

_mergeWith :: Text -> Text -> Text -> Text
_mergeWith m = (<>) . (<> m)

_showText :: Show a => a -> Text
_showText = pack . show

_divisableBy :: Integral i => i -> i -> Bool
_divisableBy n = (0 ==) . (`mod` n)

_divisable100 :: Integral i => i -> Bool
_divisable100 = _divisableBy _hundred

_div10 :: Integral i => i -> i
_div10 = (`div` _ten)

_rem10 :: Integral i => i -> i
_rem10 = (`rem` _ten)

_ten :: Integral i => i
_ten = 10

_hundred :: Integral i => i
_hundred = 100

_thousand :: Integral i => i
_thousand = 1000

_million :: Integral i => i
_million = 1000000

_billion :: Integral i => i
_billion = 1000000000

_trillion :: Integral i => i
_trillion = 1000000000000

_iLogFloor :: (Integral i, Integral j) => i -> i -> (i, j, i)
_iLogFloor b m = go b
  where go i | m < i = (m, 0, 1)
             | q < i = (q, 2 * e, j)
             | otherwise = (div q i, 2 * e + 1, j * i)
            where ~(q, e, j) = go (i*i)

_iLog :: (Integral i, Integral j) => i -> i -> Maybe j
_iLog b m = snd <$> go b
  where go i | m < i = Just (m, 0)
             | Just (q, e) <- go (i*i) = go' i q e
             | otherwise = Nothing
        go' i q e | q < i = Just (q, 2 * e)
                  | md == 0 = Just (d, 2 * e + 1)
                  | otherwise = Nothing
            where (d, md) = divMod q i

_replaceSuffix :: Int -> Text -> Text -> Text
_replaceSuffix n s = (<> s) . dropEnd n

_showIntegral :: Integral i => i -> String -> String
_showIntegral n s
    | n < 0 = '-' : _showPositive (-(fromIntegral n :: Integer)) s
    | otherwise = _showPositive n s

_showPositive :: Integral i => i -> String -> String
_showPositive n s
    | q == 0 = tl
    | otherwise = _showPositive q tl
    where (q, r) = quotRem n 10
          tl = intToDigit (fromIntegral r) : s

_genText :: Gen Text
_genText = pack <$> listOf arbitrary

_shrinkText :: Text -> [Text]
_shrinkText = liftA2 (zipWith (<>)) inits (tails . T.drop 1)
