#!/bin/zsh

set -eu

go() {
    echo "------------------- BEGIN $@ -------------------"
    echo
    stack test "$@"
    echo
    echo "-------------------   END $@ -------------------"
    echo
}

go --resolver lts-2 # GHC 7.8
go --resolver lts-6 # GHC 7.10 
go --resolver lts-7 # GHC 8.0
