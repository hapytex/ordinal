{-|
Module      : Text.Numerals.Languages
Description : A module that re-exports the algorithms to convert numbers to words in the supported languages.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module imports the /num to word/ algorithms and re-exports these algorithms. The module thus can be used to conveniently import algorithms for all supported languages.
-}


module Text.Numerals.Languages (
    dutch, english, french
  ) where

import Text.Numerals.Languages.Dutch(dutch)
import Text.Numerals.Languages.English(english)
import Text.Numerals.Languages.French(french)
