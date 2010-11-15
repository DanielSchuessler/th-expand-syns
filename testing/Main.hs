{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module Main where

import Language.Haskell.TH.ExpandSyns
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
    
-- type A = forall a. B a; type B a = Maybe a; expand [t|B A|]

type A x = [x] 
type B f = forall x. f x 
type C f = f Integer


test1 = $(
    do
      t1 <-       [t| forall a. Show a => a -> B []            -> (Int,C []) |]
      expected <- [t| forall a. Show a => a -> (forall x. [] x) -> (Int,[] Integer) |]
      report False ("expected: "++pprint expected)
      actual <- expandSyns t1
      report False ("actual: "++pprint actual)
      if (pprint expected==pprint actual) then [| putStrLn "Ok" |] else [| error "expected /= actual" |] 

 )

main = test1
