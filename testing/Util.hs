{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}
module Util where
import           Language.Haskell.TH
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


forallT' xs = forallT ((PlainTV . mkName) `fmap` xs)
forallT'' xs = forallT' xs (cxt [])
varT' = varT . mkName
conT' = conT . mkName

x --> y = (arrowT `appT` x) `appT` y
infixr 5 -->

#if !MIN_VERSION_template_haskell(2,8,0)
reportWarning = report False
#endif

