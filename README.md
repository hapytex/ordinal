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
 2. English (en);
 3. French (fr); and
 4. German (de)

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

The modules are all located under `Text.Numerals` module. The `Text.Numerals`
module exports the main modules.

The `Text.Numerals.Class` module defines classes, data types and synonyms that
provide an interface to convert numbers to words.

The `Text.Numerals.Algorithm` module contains algorithms to make converting
numbers to words more convienient. The module `Text.Numerals.Algorithm.Template`
contains functions for *template Haskell*, at the moment this only contains a
function to make an `ordinize` function.

The `Text.Numerals.Prefix` module contains numerical prefixes, at the moment
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

### Adding a new language

There is a `src/Text/Numerals/Languages/Language.hs.template` template file to
include a new language. This includes a list of tasks to do in order to add the
language. The `test/` directory contains a `test/Text/Numerals/Languages/LanguageSpec.hs.template`
file to test the new language. In these templates, one needs to fill in the
`???` parts.

Normally the languages are tested for all numbers in the 0-200 range, and the
first hundred Fibonacci numbers greater than 200, so:
```
[
  233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393
 ,196418,317811,514229,832040,1346269,2178309,3524578,5702887,9227465,14930352
 ,24157817,39088169,63245986,102334155,165580141,267914296,433494437,701408733
 ,1134903170,1836311903,2971215073,4807526976,7778742049,12586269025,20365011074
 ,32951280099,53316291173,86267571272,139583862445,225851433717,365435296162
 ,591286729879,956722026041,1548008755920,2504730781961,4052739537881,6557470319842
 ,10610209857723,17167680177565,27777890035288,44945570212853,72723460248141
 ,117669030460994,190392490709135,308061521170129,498454011879264,806515533049393
 ,1304969544928657,2111485077978050,3416454622906707,5527939700884757,8944394323791464
 ,14472334024676221,23416728348467685,37889062373143906,61305790721611591,99194853094755497
 ,160500643816367088,259695496911122585,420196140727489673,679891637638612258
 ,1100087778366101931,1779979416004714189,2880067194370816120,4660046610375530309
 ,7540113804746346429,12200160415121876738,19740274219868223167,31940434634990099905
 ,51680708854858323072,83621143489848422977,135301852344706746049,218922995834555169026
 ,354224848179261915075,573147844013817084101,927372692193078999176,1500520536206896083277
 ,2427893228399975082453,3928413764606871165730,6356306993006846248183,10284720757613717413913
 ,16641027750620563662096,26925748508234281076009,43566776258854844738105
 ,70492524767089125814114,114059301025943970552219
]
```

and powers of ten until *10<sup>24</sup>*.

---

This package is dedicated to *Wouter Folens* (\* 2019), in the hope that he will
learn about transforming numbers into words.
