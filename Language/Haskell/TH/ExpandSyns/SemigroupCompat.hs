{-# LANGUAGE CPP #-}
module Language.Haskell.TH.ExpandSyns.SemigroupCompat(Semigroup(..), Monoid(..)) where

#if MIN_VERSION_base(4,9,0)

import Data.Semigroup

#else

import Data.Monoid(Monoid(..))
import Prelude

infixr 6 <>
class Semigroup a where
  (<>) :: a -> a -> a

#endif
