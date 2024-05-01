#!/bin/sh

# if one of the steps fails then propagate
set -o errexit

stack build \
    --local-bin-path ".stack-work/dist" \
    --copy-bins \
    --flag oracle-simple:strict \
    --allow-different-user

./scripts/in-build-container/format-repo.sh

# hlint enforces style and usage rules configured in .hlint.yaml. Note that
# we use the hlint specified in our stack.yaml rather than the one provided
# by the buildpack
hlint .

# weeder runs dead code detection
# it's configuration of what is an entrypoint in the weeder.dhall file
weeder
