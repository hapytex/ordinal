{-# LANGUAGE TemplateHaskellQuotes #-}

module Text.Numerals.Algorithm.Template (
    ordinizeFromDict
  ) where

import Control.Applicative(liftA2)

import Data.Map.Strict(Map, elems, fromListWith)
import Data.Text(Text, isSuffixOf, pack, snoc)

import Text.Numerals.Internal(_replaceSuffix)

import Language.Haskell.TH(Body(GuardedB), Clause(Clause), Dec(FunD), Exp(AppE, ConE, LitE, VarE), Guard(NormalG), Lit(CharL, IntegerL, StringL), Pat(VarP), mkName)

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

ordinizeFromDict :: String -> [(String, String)] -> Dec
ordinizeFromDict nm ts = FunD (mkName nm) [Clause [VarP t] (GuardedB (map _toGuard (elems (_ordinizeMap t' ts)) ++ [(NormalG (ConE 'True), t')])) []]
    where t = mkName "t"
          t' = VarE t
