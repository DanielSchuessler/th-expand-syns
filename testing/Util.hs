{-# LANGUAGE TemplateHaskell #-}
module Util where
import Language.Haskell.TH
import Language.Haskell.TH.ExpandSyns

mkTest ::  Ppr a => Q Type -> Q a -> Q Exp
mkTest input expected =
    do
      input' <- input 
      expected' <- expected 
      report False ("info: expected = "++pprint expected')
      actual <- expandSyns input'
      report False ("info:  actual  = "++pprint actual)
      if (pprint expected'==pprint actual) then [| putStrLn "Ok" |] else [| error "expected /= actual" |] 


forallT' xs = forallT ((PlainTV . mkName) `fmap` xs) 
forallT'' xs = forallT' xs (cxt []) 
varT' = varT . mkName
conT' = conT . mkName

x --> y = (arrowT `appT` x) `appT` y
infixr 5 -->
