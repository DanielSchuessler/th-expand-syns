{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# OPTIONS -ddump-splices #-}

import Language.Haskell.TH.ExpandSyns
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Util
import Types    


main = do
    putStrLn "Basic test..."
    $(mkTest  [t| forall a. Show a => a -> ForAll [] -> (Int,ApplyToInteger []) |] 
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
