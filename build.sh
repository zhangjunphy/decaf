#!/bin/bash -eu

declare -r TOP="$(git rev-parse --show-toplevel)"
declare -r GREP="grep --quiet --no-messages"

cd "$TOP"

stack solver
stack exec alex -- --ghc --template=$TOP/alex $TOP/src/Scanner.x
# Inform happy to create the state diagram file and print debug information
stack exec happy -- -i -a -d $TOP/src/Parser.y
stack build

rm -f $TOP/src/Scanner.hs $TOP/src/Parser.hs
