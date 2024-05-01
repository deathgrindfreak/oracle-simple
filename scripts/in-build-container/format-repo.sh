#!/bin/sh

find * -name '*.hs' | xargs -P0 fourmolu --no-cabal -i
