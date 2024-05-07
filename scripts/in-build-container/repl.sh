#!/bin/sh

# Create a .ghci file on the fly to allow a default prompt
# ( thank Scott Conley for this trick :) )
echo ':set prompt "\ESC[32m\STXrepl> \ESC[m\STX"' > ~/.ghci
chown root:root ~/.ghci
chmod 700 ~/.ghci

stack repl \
    #--main-is oracle-simple:lib \
    --ghci-options '-j +RTS -A32m -RTS -fno-write-ide-info -flocal-ghci-history' \
    oracle-simple:lib oracle-simple:exe:functional-tests
