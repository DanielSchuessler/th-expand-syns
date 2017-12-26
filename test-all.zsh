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

go --resolver lts-2  # GHC 7.8.4
go --resolver lts-6  # GHC 7.10.3
go --resolver lts-9  # GHC 8.0.2
go --resolver lts-10 # GHC 8.2.2
