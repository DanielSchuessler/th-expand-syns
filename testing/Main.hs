{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module Tests where

import ExpandSyns
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
    
-- type A = forall a. B a; type B a = Maybe a; expand [t|B A|]

type A x = [x] 
type B f = forall x. f x 
type C f = f Integer


foo = $(    lift . show 
            =<< expandSyns 
            =<< [t| (forall a. Show a => a -> B [] -> (Int,C [])) |]
       )
