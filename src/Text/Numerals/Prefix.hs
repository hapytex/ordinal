{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Text.Numerals.Prefix
-- Description : A module used to define /numeric prefixes/ for /long/ and /short scales/.
-- Maintainer  : hapytexeu+gh@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A module that defines /Latin/ prefixes. These prefixes are used to construct names for the /long/ and /short scales/.
-- So the /m/, /b/, /tr/ in /million/, /billion/, /trillion/.
module Text.Numerals.Prefix
  ( -- * Latin prefixes
    latinPrefixes,
    latinPrefixes',
    latinPrefix,
  )
where

import Data.Text (Text)
import Data.Vector (Vector, fromList, (!?))

-- | A list of /Latin/ prefixes, used for the /long/ and /short scale/.
latinPrefixes' ::
  -- | A list of 'Text' objects. This makes explicit recursion more convenient.
  [Text]
latinPrefixes' =
  [ "m",
    "b",
    "tr",
    "quadr",
    "quint",
    "sext",
    "sept",
    "oct",
    "non",
    "dec",
    "undec",
    "duodec",
    "tredec",
    "quattuordec",
    "quindec",
    "sedec",
    "septendec",
    "octodec",
    "novendec",
    "vigint",
    "unvigint",
    "duovigint",
    "tresvigint",
    "quattuorvigint",
    "quinvigint",
    "sesvigint",
    "septemvigint",
    "octovigint",
    "novemvigint",
    "trigint",
    "untrigint",
    "duotrigint",
    "trestrigint",
    "quattuortrigint",
    "quintrigint",
    "sestrigint",
    "septentrigint",
    "octotrigint",
    "noventrigint",
    "quadragint"
  ]

-- | The /Latin/ prefixes in a 'Vector' for /O(1)/ lookup.
latinPrefixes ::
  -- | A 'Vector' of 'Text' objects to allow fast lookup.
  Vector Text
latinPrefixes = fromList latinPrefixes'

-- | Lookup the given /Latin/ prefix for the given value.
latinPrefix ::
  Integral i =>
  -- | The value to map on a Latin prefix.
  i ->
  -- | The corresponding Latin prefix, given this exists.
  Maybe Text
latinPrefix n = latinPrefixes !? (fromIntegral n - 1)
