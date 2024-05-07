#!/bin/sh

if [ "$(stack exec which ghcid)" = "" ]; then
  stack install ghcid
fi;

stack exec ghcid -- \
  --command="stack ghci --ghc-options='-j -fno-write-ide-info' oracle-simple:lib oracle-simple:exe:functional-tests"
