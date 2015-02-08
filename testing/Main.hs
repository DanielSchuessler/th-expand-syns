{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
-- {-# OPTIONS -ddump-splices #-}

import Language.Haskell.TH.ExpandSyns
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Util
import Types    


main = do
    putStrLn "Basic test..."
    $(mkTest  [t| forall a. Show a => a -> ForAll [] -> (Int,ApplyToInteger []) |] 

-- GHC 7.8 always seems to consider the body of 'ForallT' to have a 'PlainTV', 
-- whereas it always has a 'KindedTV' with GHC 7.10 (in both cases, it doesn't appear 
-- to matter whether the definition of 'ForAll' is actually written with a kind signature).
#if MIN_VERSION_template_haskell(2,10,0)
              [t| forall a. Show a => a -> (forall (x :: *). [] x) -> (Int,[] Integer) |]
#else
              [t| forall a. Show a => a -> (forall x. [] x) -> (Int,[] Integer) |]
#endif
              
              )

    putStrLn "Variable capture avoidance test..."
    $(let

-- See comment about 'PlainTV'/'KindedTV' above
#if MIN_VERSION_template_haskell(2,10,0)
        y_0 = KindedTV (mkName "y_0") StarT
#else
        y_0 = PlainTV (mkName "y_0")
#endif

        expectedExpansion =
         forallT 
            [y_0] 
            (cxt [])
            (conT ''Either `appT` varT' "y" `appT` varT' "y_0" --> conT ''Int)

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
             
    putStrLn "Testing that the args of type family applications are handled" 
    $(mkTest [t| (DF1 Int', TF1 Int', AT1 Int') |]
             [t| (DF1 Int, TF1 Int, AT1 Int) |])

    putStrLn "Higher-kinded synonym"
    $(mkTest 
        [t| Either' (ListOf Int') (ListOf Char) |]
        [t| Either [Int] [Char] |])

    putStrLn "Nested"
    $(mkTest 
        [t| Int'' |]
        [t| Int |])
