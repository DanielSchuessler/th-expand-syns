{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE KindSignatures #-}
module Types where

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Util

-- type A = forall a. B a; type B a = Maybe a; expand [t|B A|]

type ListOf x = [x]
type ForAll f = forall x. f x
type ApplyToInteger f = f Integer
type Int' = Int
type Either' = Either
type Int'' = Int
type Id a = a

-- type E x = forall y. Either x y -> Int
$(sequence [tySynD (mkName "E") [plainTV (mkName "x")]
                (forallT'' ["y"] (conT ''Either `appT` varT' "x" `appT` varT' "y" --> conT ''Int))
           ])


data family DF1 a

data instance DF1 Int = DInt (ListOf ())

type family TF1 a

type instance TF1 Int = ListOf ()

class Class1 a where
    type AT1 a

instance Class1 Int where type AT1 Int = ListOf ()
