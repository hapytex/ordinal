{-# LANGUAGE RankNTypes, TemplateHaskellQuotes #-}

module Text.Numerals.Algorithm where

import Data.Foldable(toList)
import Data.List(sortOn)
import Data.Maybe(maybe)
import Data.Text(Text, cons, pack, isSuffixOf)
import Data.Vector(Vector, (!), fromList)
import qualified Data.Vector as V

import Text.Numerals.Internal(_replaceSuffix)

import Language.Haskell.TH(Body(GuardedB), Clause(Clause), Dec(FunD), Exp(AppE, ConE, LitE, VarE), Guard(NormalG), Lit(IntegerL, StringL), Pat(VarP), mkName)

type MergerFunction i = i -> i -> Text -> Text -> Text

data NumeralsAlgorithm = NumeralsAlgorithm {
    minusWord :: Text
  , oneWord :: Text
  , lowWords :: Vector Text
  , midWords :: [(Int, Text)]
  , mergeFunction :: forall i . Integral i => MergerFunction i
  , ordinize :: Text -> Text
  }

numeralsAlgorithm :: (Foldable f, Foldable g) => Text -> Text -> Text -> f Text -> g (Int, Text) -> (forall i . Integral i => MergerFunction i) -> (Text -> Text) -> NumeralsAlgorithm
numeralsAlgorithm minus zero one lowWords midWords = NumeralsAlgorithm minus one (fromList (zero : one : toList lowWords)) ((sortOn (negate . fst) . toList) midWords)

data NumberSegment i = NumberSegment {
      segmentDivision :: MNumberSegment i
    , segmentValue :: i
    , segmentText :: Text
    , segmentRemainder ::  MNumberSegment i
    } deriving (Eq, Ord, Read, Show)

type MNumberSegment i = Maybe (NumberSegment i)

maybeSegment :: Integral i => (i -> NumberSegment i) -> i -> MNumberSegment i
maybeSegment f = go
    where go 0 = Nothing
          go i = Just (f i)

toSegmentLow' :: Integral i => Vector Text -> i -> NumberSegment i
toSegmentLow' vs = go
    where go i | i >= nvs = NumberSegment (Just (go dv)) nvs lv (tl md)
               | otherwise = NumberSegment Nothing i (vs ! fromIntegral i) Nothing
               where (dv, md) = divMod i nvs
          lv = V.last vs
          nvs = fromIntegral (V.length vs) - 1
          tl = maybeSegment go

toSegments' :: Integral i => Vector Text -> [(Int, Text)] -> i -> NumberSegment i
toSegments' lows = go
    where go [] n = toSegmentLow' lows n
          go ma@((m, v) : ms) n
              | im > n = go ms n
              | dv == 1 = NumberSegment Nothing im v tl
              | otherwise = NumberSegment (Just (go ma dv)) im v tl
              where im = fromIntegral m
                    (dv, md) = divMod n im
                    tl = maybeSegment (go ms) md

compressSegments' :: Integral i => Text -> MergerFunction i -> NumberSegment i -> Text
compressSegments' one' merger = snd . go
    where go (NumberSegment dv' i t md') = _mergeTail md' (dvi * i, merger dvi i dv t)
              where (dvi, dv) = _unwrap dv'
          _unwrap = maybe (1, one') go
          _mergeTail Nothing r = r
          _mergeTail (Just md') (vi, v) = (vi + mdi, merger vi mdi v md)
              where (mdi, md) = go md'

toCardinal :: Integral i => NumeralsAlgorithm -> i -> Text
toCardinal NumeralsAlgorithm { minusWord=minusWord, oneWord=oneWord, lowWords=lowWords, midWords=midWords, mergeFunction=mergeFunction } = cardinal
    where cardinal i
              | i < 0 = minusWord <> cons ' ' (go (-i))
              | otherwise = go i
              where go = compressSegments' oneWord mergeFunction . toSegments' lowWords midWords

toOrdinal :: Integral i => NumeralsAlgorithm -> i -> Text
toOrdinal na@NumeralsAlgorithm { ordinize=ordinize } = ordinize . toCardinal na

_getPrefix :: [Char] -> [Char] -> (Int, [Char])
_getPrefix [] bs = (0, bs)
_getPrefix aa@(a:as) ba@(b:bs)
     | a == b = _getPrefix as bs
     | otherwise = (length aa, ba)

_packText :: String -> Exp
_packText = AppE (VarE 'pack) . LitE . StringL

_packExp :: Int -> String -> Exp -> Exp
_packExp 0 sc nm = AppE (AppE (VarE '(<>)) nm) (_packText sc)
_packExp l sc nm = AppE (AppE (AppE (VarE '_replaceSuffix) (LitE (IntegerL (fromIntegral l)))) (_packText sc)) nm

ordinizeSingle :: Exp -> String -> String -> (Guard, Exp)
ordinizeSingle nm sa sb = (NormalG (AppE (AppE (VarE 'isSuffixOf) (_packText sa)) nm), _packExp l sc nm)
    where (l, sc) = _getPrefix sa sb

ordinizeFromDict :: [(String, String)] -> Dec
ordinizeFromDict ts = FunD (mkName "ordinize'") [Clause [VarP t] (GuardedB (map (uncurry (ordinizeSingle t')) ts ++ [(NormalG (ConE 'True), t')])) []]
    where t = mkName "t"
          t' = VarE t
