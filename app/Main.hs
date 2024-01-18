{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char (digitToInt, isDigit)
import Data.Default.Class (Default (def))
import Data.HashMap.Strict (HashMap, fromList, lookup)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, toCaseFold)
import qualified Data.Text.IO as TI
import System.Console.GetOpt (ArgDescr (NoArg, ReqArg), ArgOrder (RequireOrder), OptDescr (Option), getOpt, usageInfo)
import System.Environment (getArgs)
import Text.Numerals.Algorithm (NumeralsAlgorithm)
import Text.Numerals.Class (toCardinal, toOrdinal, toShortOrdinal, toTimeText')
import Text.Numerals.Languages (dutch, english, french, german)
import Prelude hiding (lookup)

languages_ :: [([Text], NumeralsAlgorithm)]
languages_ =
  [ (["nl", "nld", "dut", "dutch"], dutch),
    (["en", "eng", "english"], english),
    (["fr", "fra", "fre", "french"], french),
    (["de", "deu", "ger", "german"], german)
  ]

languages :: HashMap Text NumeralsAlgorithm
languages = fromList [(toCaseFold k, v) | (ks, v) <- languages_, k <- ks]

data OrdinalMode = Cardinal | Ordinal | ShortOrdinal | Time deriving (Eq, Ord, Read, Show)

data OrdinalConfig = OrdinalConfig {lang :: NumeralsAlgorithm, mode :: OrdinalMode, help :: Bool}

instance Default OrdinalMode where
  def = Cardinal

instance Default OrdinalConfig where
  def = OrdinalConfig english def False

determine :: OrdinalMode -> NumeralsAlgorithm -> Integer -> Text
determine Cardinal = toCardinal
determine Ordinal = toOrdinal
determine ShortOrdinal = toShortOrdinal
determine Time = go
  where
    go lng x = toTimeText' lng (fromIntegral (x `div` 60)) (fromIntegral (x `mod` 60))

findLanguage :: String -> IO NumeralsAlgorithm
findLanguage k =
  case lookup (toCaseFold (pack k)) languages of
    Just x -> pure x
    _ -> fail ("Can not find language \"" ++ k ++ "\".")

options :: [OptDescr (OrdinalConfig -> IO OrdinalConfig)]
options =
  [ Option "l" ["lang", "language"] (ReqArg (\l' v -> (\l -> v {lang = l}) <$> findLanguage l') "lang") "specify the language, English by default",
    Option "c" ["cardinal"] (NoArg (\v -> pure (v {mode = Cardinal}))) "set the number mode to cardinal numbers, which is the deault",
    Option "o" ["ordinal"] (NoArg (\v -> pure (v {mode = Ordinal}))) "set the number mode to ordinal numbers",
    Option "s" ["short-ordinal"] (NoArg (\v -> pure (v {mode = ShortOrdinal}))) "set the number mode to short ordinal numbers",
    Option "t" ["time"] (NoArg (\v -> pure (v {mode = Time}))) "set the number mode to time",
    Option "?h" ["help"] (NoArg (\v -> pure (v {help = True}))) "show this help page"
  ]

header :: String
header = "Usage: ordinal [OPTION...] numbers..."

compilerOpts :: [String] -> IO ([OrdinalConfig -> IO OrdinalConfig], [String])
compilerOpts argv =
  case getOpt RequireOrder options argv of
    (o, n, []) -> return (o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))

readInt :: String -> Integer
readInt v = fromMaybe (read v) (go 0 v)
  where
    go h (':' : xs) = Just (60 * h + read xs)
    go h (x : xs) | isDigit x = go (h * 10 + fromIntegral (digitToInt x)) xs
    go _ _ = Nothing

main :: IO ()
main = do
  args <- getArgs
  ~(opts, xs) <- compilerOpts args
  opt <- foldl (>>=) def opts
  if help opt
    then putStrLn (usageInfo header options)
    else mapM_ (TI.putStrLn . determine (mode opt) (lang opt) . readInt) xs
