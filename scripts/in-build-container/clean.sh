#! /bin/sh

set -e

# hedgehog test output includes unicode that errors on the debian buildpack container
export LANG="C.UTF-8"

stack clean
