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

go --resolver lts-9  # GHC 8.0.2
go --resolver lts-10 # GHC 8.2.2
go --resolver lts-12 # GHC 8.4.4
go --resolver lts-14 # GHC 8.6.5
go --stack-yaml stack-8.8.yaml
