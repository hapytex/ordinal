#!/bin/bash

edittmp () {
  cp "$1$2.hs.template" "$1$lng$3.hs"
  git add "$1$lng$3.hs"
  editor "$1$lng$3.hs"
}

if [ "$#" -lt '1' ]; then
  echo >&2 "Please specify the name of the language in PerlCase."
  exit 1
fi

lng="$1"
lsb="$(tr '[:upper:]' '[:lower:]' <<<$lng)"

git stash
git checkout master
git checkout -b "lang/$lsb"

if [ -f "src/Text/Numerals/Languages/$lng.hs" ]; then
  echo >&2 "Language $lng already exists."
  exit 2
fi

edittmp 'src/Text/Numerals/Languages/' 'Language' ''
edittmp 'test/Text/Numerals/Languages/' 'LanguageSpec' 'Spec'

editor ordinal.cabal
editor README.md
