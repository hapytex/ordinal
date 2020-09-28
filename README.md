# ordinal

[![Build Status of the package by Travis](https://travis-ci.com/hapytex/ordinal.svg?branch=master)](https://travis-ci.com/hapytex/ordinal)
[![Build Status of the package by Hackage](https://matrix.hackage.haskell.org/api/v2/packages/ordinal/badge)](https://matrix.hackage.haskell.org/#/package/ordinal)
[![Hackage version badge](https://img.shields.io/hackage/v/ordinal.svg)](https://hackage.haskell.org/package/ordinal)

A package to convert numbers to the words. It contains a datatype for
algorithmic conversion that can convert the number for *most* languages.
It converts numbers to its *cardinal* and *ordinal* format.

The package is based on Python's [**`num2words`** package \[GitHub\]](https://github.com/savoirfairelinux/num2words).

The following languages are currently supported (in *alphabetical* order):

 1. Dutch (nl);
 2. English (en); and
 3. French (fr)

## Usage

One can import the `Text.Numerals` module, and use the `toCardinal`
and `toOrdinal` functions with a number-to-word algorithm that is exported by
the `Text.Numerals.Languages` module, for example:

```
Prelude Text.Numerals Data.Text.IO> Data.Text.IO.putStrLn (toCardinal english 42)
forty-two
Prelude Text.Numerals Data.Text.IO> Data.Text.IO.putStrLn (toOrdinal french 42)
quarante-deuxième
```

One can also define a language algorithm themselves, for this one can look at
the source code of the language modules.

## Package structure

The modules are all located under `Text.Numerals` module. The Text.Numerals`
module exports the main modules.

The `Text.Numerals.Class` module defines classes, data types and synonyms that
provide an interface to convert numbers to words.

The `Text.Numerals.Algorithm` module contains algorithms to make converting
numbers to words more convienient. The module `Text.Numerals.Algorithm.Template`
contains functions for *template Haskell*, at the moment this only contains a
function to make an `ordinize` function.

The `Text.Numerals.Prefix` module contains numerical prefixes. At the moment
only *Latin* prefixes. These are used for *short scale* and *long scale*
algorithms to specify millions, billions, etc. in languages.

The `Text.Numerals.Languages` module exports for each implemented language its
algorithm, this makes working with multiple languages more convienient. Under
this module there are dedicated modules per language that do not only export the
algorithm for that language, but also helper functions and constants.

## `ordinal` is not *safe* Haskell

The package uses the `Data.Vector` module which is not safe, and therefore
`ordinal` is not safe either.

## Contribute

You can contribute by making a pull request on the [*GitHub
repository*](https://github.com/hapytex/ordinal).

You can contact the package maintainer by sending a mail to
[`hapytexeu+gh@gmail.com`](mailto:hapytexeu+gh@gmail.com).

---

This package is dedicated to *Wouter Folens* (\* 2019), in the hope that he will
learn about transforming numbers into words.
