{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
module Util where
import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype.TyVarBndr
import           Language.Haskell.TH.ExpandSyns

mkTest ::  Q Type -> Q Type -> Q Exp
mkTest input expected =
    do
      input' <- input
      runIO . putStrLn $ ("info: input = "++show input')
      expected' <- expected
      runIO . putStrLn $ ("info: expected = "++show expected')
      actual <- expandSyns input'
      runIO . putStrLn $ ("info:  actual  = "++show actual)
      if (pprint expected'==pprint actual) then [| putStrLn "Ok" |] else [| error "expected /= actual" |]


forallT' :: [String] -> Q Cxt -> Q Type -> Q Type
forallT' xs = forallT ((plainTVSpecified . mkName) `fmap` xs)

forallT'' :: [String] -> Q Type -> Q Type
forallT'' xs = forallT' xs (cxt [])

varT' :: String -> Q Type
varT' = varT . mkName

conT' :: String -> Q Type
conT' = conT . mkName

(-->) :: Q Type -> Q Type -> Q Type
x --> y = (arrowT `appT` x) `appT` y
infixr 5 -->

#if !MIN_VERSION_template_haskell(2,8,0)
reportWarning :: String -> Q ()
reportWarning = report False
#endif

