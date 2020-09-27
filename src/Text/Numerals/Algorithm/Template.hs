{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
Module      : Text.Numerals.Algorithm.Template
Description : A module that constructs template Haskell to make defining an ordinize function more convenient.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

The module is designed to construct an 'Exp' based on the mapping data provided. It will check if the text object
ends with the given suffix, and replace the suffix with another suffix. It aims to compile this into an efficient function.
-}

module Text.Numerals.Algorithm.Template (
    ordinizeFromDict
  ) where

import Data.Map.Strict(Map, elems, fromListWith)
import Data.Text(isSuffixOf, pack, snoc)

import Text.Numerals.Internal(_replaceSuffix)

import Language.Haskell.TH(Body(GuardedB), Clause(Clause), Dec(FunD), Exp(AppE, ConE, LitE, VarE), Guard(NormalG), Lit(CharL, IntegerL, StringL), Name, Pat(VarP), mkName)

_getPrefix :: [Char] -> [Char] -> (Int, [Char])
_getPrefix [] bs = (0, bs)
_getPrefix aa@(a:as) ba@(b:bs)
     | a == b = _getPrefix as bs
     | otherwise = (length aa, ba)

_orCondition :: [Exp] -> Guard
_orCondition [] = NormalG (ConE 'False)
_orCondition xs = NormalG (foldr1 (AppE . AppE (VarE '(||))) xs)

_packText :: String -> Exp
_packText = AppE (VarE 'pack) . LitE . StringL

_packExp :: Int -> String -> Exp -> Exp
_packExp 0 [] nm = nm
_packExp 0 [s] nm = AppE (AppE (VarE 'snoc) nm) (LitE (CharL s))
_packExp 0 sc nm = AppE (AppE (VarE '(<>)) nm) (_packText sc)
_packExp l sc nm = AppE (AppE (AppE (VarE '_replaceSuffix) (LitE (IntegerL (fromIntegral l)))) (_packText sc)) nm

_ordinizeSingle :: Exp -> String -> String -> ((Int, String), ([Exp], Exp))
_ordinizeSingle nm sa sb = (p, ([AppE (AppE (VarE 'isSuffixOf) (_packText sa)) nm], _packExp l sc nm))
    where p@(l, sc) = _getPrefix sa sb

_ordinizeMap :: Exp -> [(String, String)] -> Map (Int, String) ([Exp], Exp)
_ordinizeMap n = fromListWith f . map (uncurry (_ordinizeSingle n))
    where f (as, a) (bs, _) = (bs ++ as, a)

_toGuard :: ([Exp], Exp) -> (Guard, Exp)
_toGuard (gs, es) = (_orCondition gs, es)

-- | Construct a function with the given name that maps suffixes in the first
-- item of the 2-tuples to the second item of the 2-tuples. It turns this into a
-- declaration.
ordinizeFromDict
  :: String  -- ^ The name of the function, often this is just @ordinize'@
  -> [(String, String)]  -- ^ The list of suffixes and their corresponding mapping, the suffixes should be non-overlapping.
  -> Name  -- ^ The name of the post-processing function in case there was no match, one can for example use 'id'.
  -> Dec  -- ^ The corresponding declaration.
ordinizeFromDict nm ts pp = FunD (mkName nm) [Clause [VarP t] (GuardedB (map _toGuard (elems (_ordinizeMap t' ts)) ++ [(NormalG (ConE 'True), AppE (VarE pp) t')])) []]
    where t = mkName "t"
          t' = VarE t
