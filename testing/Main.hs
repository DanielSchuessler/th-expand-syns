{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

import Language.Haskell.TH.Datatype.TyVarBndr
import Language.Haskell.TH.ExpandSyns
import Language.Haskell.TH
import Util
import Types

main :: IO ()
main = do
    putStrLn "Basic test..."
    $(mkTest  [t| forall a. Show a => a -> ForAll [] -> (Int,ApplyToInteger []) |]

-- GHC 7.8 always seems to consider the body of 'ForallT' to have a 'PlainTV',
-- whereas it always has a 'KindedTV' with GHC 7.10 (in both cases, it doesn't appear
-- to matter whether the definition of 'ForAll' is actually written with a kind signature).
              [t| forall a. Show a => a -> (forall (x :: *). [] x) -> (Int,[] Integer) |])

    putStrLn "Variable capture avoidance test..."
    $(let

-- See comment about 'PlainTV'/'KindedTV' above
        y_0 = kindedTVSpecified (mkName "y_0") StarT

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

    putStrLn "Synonyms in kinds"
    $(mkTest
        (sigT (conT ''Int) (ConT ''Id `AppT` StarT))
        (sigT (conT ''Int) StarT))

    $(do
        reportWarning "No warning about type families should appear after this line." -- TODO: Automate this test with a custom Quasi instance?
        _ <- expandSynsWith noWarnTypeFamilies =<< [t| (DF1 Int', TF1 Int', AT1 Int') |]
        [| return () |])



