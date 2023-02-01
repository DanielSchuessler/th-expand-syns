## 0.4.11.0 [2023.01.31]

* Support `TypeDataD` when building with `template-haskell-2.20.0.0` (GHC 9.6)
  or later.

## 0.4.10.0 [2022.07.23]

* Support `DefaultD`, `PromotedInfixT`, and `PromotedUInfixT` when building
  with `template-haskell-2.19.0.0` (GHC 9.4) or later.

## 0.4.9.0 [2021.08.30]

* Consolidate the type-synonym expansion functionality with `th-abstraction`,
  which also provides the ability to expand type synonyms. After this change,
  the `th-expand-syns` library is mostly a small shim on top of
  `th-abstraction`. The only additional pieces of functionality that
  `th-expand-syns` which aren't currently available in `th-abstraction` are:

  * `th-expand-syns`' `expandSyns{With}` functions will warn that they cannot
    expand type families (if the `SynonymExpansionSettings` are configured to
    check for this). By contrast, `th-abstraction`'s `applySubstitution`
    function will silently ignore type families.
  * `th-expand-syns` provides a `substInCon` function which allows substitution
    into `Con`s.
  * `th-expand-syns` provides `evade{s}` functions which support type variable
    `Name` freshening that calculating the free variables in any type that
    provides an instance of `Data`.

## 0.4.8.0 [2021.03.12]

* Make the test suite compile with GHC 9.0 or later.
* Drop support for pre-7.0 versions of GHC.

## 0.4.7.0

* Support GHC 9.0 / template-haskell-2.17 (Thanks to @mgsloan)

## 0.4.5.0

* Support GHC 8.8 / template-haskell-2.15 (Thanks to Ryan Scott)
* Support GHC 8.6 / template-haskell-2.14 (Thanks to Chaitanya Koparkar)

## 0.4.4.0

*   Made `SynonymExpansionSettings` an instance of `Semigroup` (fixes build with GHC 8.4.1 alpha).

## 0.4.3.0

*   Added support for GHC 8.2.1 / template-haskell-2.12 (Thanks to Ryan Scott)

## 0.4.2.0

*   Eliminated warnings about unrecognized results of 'reify'.

## 0.4.1.0

*   Added a setting for suppressing warnings about type families.

## 0.4.0.0

*   Fixed build with GHC 8 / template-haskell-2.11 (Thanks to Christiaan Baaij)

    Note: `substInCon` doesn't support GADT constructors with GHC 8 in this version

## 0.3.0.6

*   Fixed build with current (commit 029a296a770addbd096bbfd6de0936327ee620d4) GHC 7.10 (Thanks to David Fox)

## 0.3.0.5

*   Fixed build with GHC 7.10.1-rc2 / template-haskell-2.10 (Thanks to Gabor Greif)
