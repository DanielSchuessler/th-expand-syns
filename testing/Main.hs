{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# OPTIONS -ddump-splices #-}
module Main where

import Language.Haskell.TH.ExpandSyns
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Util
    
-- type A = forall a. B a; type B a = Maybe a; expand [t|B A|]

type A x = [x] 
type B f = forall x. f x 
type C f = f Integer


$(sequence [tySynD (mkName "E") [PlainTV (mkName "x")]  
                (forallT'' ["y"] (conT ''Either `appT` varT' "x" `appT` varT' "y" --> conT ''Int))
           ])


data family DF1 a

data instance DF1 Int = DInt (A ())

type family TF1 a

type instance TF1 Int = A ()

class Class1 a where
    type AT1 a

instance Class1 Int where type AT1 Int = A ()

main = do
    putStrLn "Basic test..."
    $(mkTest  [t| forall a. Show a => a -> B []             -> (Int,C []) |] 
              [t| forall a. Show a => a -> (forall x. [] x) -> (Int,[] Integer) |])

    putStrLn "Variable capture avoidance test..."
    $(let
        expectedExpansion =
         forallT'' ["y_0"] (conT ''Either `appT` varT' "y" `appT` varT' "y_0" --> conT ''Int)
         -- the naive (and wrong) result would be:
         --   forall y. (forall y. Either y y -> Int)
      in
        mkTest  (forallT'' ["y"] (conT' "E" `appT` varT' "y")) 
                (forallT'' ["y"] expectedExpansion))

    putStrLn "Testing that it doesn't crash on type families (expanding them is not supported yet)"
    $(let
        t = [t| (DF1 Int, TF1 Int, AT1 Int) |]
      in
        mkTest t t)
             
