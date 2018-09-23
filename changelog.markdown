## next

* Support GHC 8.6 / template-haskell-2.14

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
