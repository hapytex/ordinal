-- |
-- Module      : Text.Numerals
-- Description : The main module that converts numbers to words. This module re-exports a set of modules.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- The main module of the @ordinal@ package. This module re-exports the most important modules of the package to convert numbers to words in the supported languages and functions to construct algorithmic transformations.
module Text.Numerals
  ( module Text.Numerals.Algorithm,
    module Text.Numerals.Class,
    module Text.Numerals.Languages,
    module Text.Numerals.Prefix,
  )
where

import Text.Numerals.Algorithm
import Text.Numerals.Class
import Text.Numerals.Languages
import Text.Numerals.Prefix
