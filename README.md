# ordinal
[![Build Status of the package by Travis](https://travis-ci.com/hapytex/ordinal.svg?branch=master)](https://travis-ci.com/hapytex/ordinal)
[![Build Status of the package by Hackage](https://matrix.hackage.haskell.org/api/v2/packages/ordinal/badge)](https://matrix.hackage.haskell.org/#/package/ordinal)
[![Hackage version badge](https://img.shields.io/hackage/v/ordinal.svg)](https://hackage.haskell.org/package/ordinal)

A package to convert numbers to the words. It contains a datatype for
algorithmic conversion that can convert the number for *most* languages.
It converts numbers to its *canonical* and *ordinal* format.

The package is based on Python's [**`num2words`** package \[GitHub\]](https://github.com/savoirfairelinux/num2words).

## `ordinal` is not *safe* Haskell

The package uses the `Data.Vector` module which is not safe, and therefore
`ordinal` is not safe either.

## Contribute

You can contribute by making a pull request on the [*GitHub
repository*](https://github.com/hapytex/ordinal).

You can contact the package maintainer by sending a mail to
[`hapytexeu+gh@gmail.com`](mailto:hapytexeu+gh@gmail.com).

