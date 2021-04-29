# `th-expand-syns`

[![Hackage](https://img.shields.io/hackage/v/th-expand-syns.svg)](https://hackage.haskell.org/package/th-expand-syns)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/th-expand-syns.svg)](http://packdeps.haskellers.com/reverse/th-expand-syns)
[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![Build Status](https://github.com/DanielSchuessler/th-expand-syns/workflows/Haskell-CI/badge.svg)](https://github.com/DanielSchuessler/th-expand-syns/actions?query=workflow%3AHaskell-CI)

[Hackage: th-expand-syns]:
  http://hackage.haskell.org/package/th-expand-syns
  "th-expand-syns package on Hackage"
[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"

Expands type synonyms in Template Haskell ASTs.

As of version `0.4.9.0`, this library is a small shim on top of the
`applySubstitution`/`resolveTypeSynonyms` functions from `th-abstraction`, so
you may want to consider using `th-abstraction` instead. The only pieces of
functionality that `th-expand-syns` provides which are not currently present in
`th-abstraction` are:

* `th-expand-syns`' `expandSyns{With}` functions will warn that they cannot
  expand type families (if the `SynonymExpansionSettings` are configured to
  check for this). By contrast, `th-abstraction`'s `applySubstitution`
  function will silently ignore type families.
* `th-expand-syns` provides a `substInCon` function which allows substitution
  into `Con`s.
* `th-expand-syns` provides `evade{s}` functions which support type variable
  `Name` freshening that calculating the free variables in any type that
  provides an instance of `Data`.
