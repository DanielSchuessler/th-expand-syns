{-# OPTIONS -Wall #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Haskell.TH.ExpandSyns(-- * Expand synonyms
                                      expandSyns
                                      -- * Misc utilities
                                     ,substInType,evades,evade) where
    
import Language.Haskell.TH hiding(cxt)
import Data.Set as Set    
import Data.Generics


(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap

type SynInfo = ([Name],Type)

nameIsSyn :: Name -> Q (Maybe SynInfo)
nameIsSyn n = do
  i <- reify n
  case i of
    TyConI d -> return (decIsSyn d)
    ClassI _ -> return Nothing
    PrimTyConI _ _ _ -> return Nothing
    _ -> fail ("nameIsSyn: unexpected info: "++show(n,i))

decIsSyn :: Dec -> Maybe SynInfo
decIsSyn (ClassD _ _ _ _ _) = Nothing
decIsSyn (DataD _ _ _ _ _) = Nothing
decIsSyn (NewtypeD _ _ _ _ _) = Nothing
decIsSyn (TySynD _ vars t) = Just (vars,t)
decIsSyn x = error ("decIsSyn: unexpected dec: "++show x)

expandSyns :: Type -> Q Type
expandSyns = 
    (\t ->
         do
           (acc,t') <- go [] t
           return (foldl AppT t' acc))


    where
      go acc ListT = return (acc,ListT)
      go acc ArrowT = return (acc,ArrowT)
      go acc x@(TupleT _) = return (acc, x)
      go acc x@(VarT _) = return (acc, x)
                          
      go [] (ForallT ns cxt t) = do
        cxt' <- mapM expandSyns cxt
        t' <- expandSyns t
        return ([],ForallT ns cxt' t')

      go acc x@(ForallT _ _ _) = 
          fail ("Unexpected application of the local quantification: "
                ++show x
                ++"\n    (to the arguments "++show acc++")")
                                  
      go acc (AppT t1 t2) = 
          do
            r <- expandSyns t2
            go (r:acc) t1
            
      go acc x@(ConT n) =
          do
            i <- nameIsSyn n
            case i of
              Nothing -> return (acc,x)
              Just (vars,body) ->
                  if length acc < length vars
                  then fail ("expandSyns: Underapplied type synonym:"++show(n,acc))
                  else 
                      let
                          substs = zip vars acc
                          expanded = foldr substInType body substs
                      in
                        go (drop (length vars) acc) expanded
                        
                      



substInType :: (Name, Type) -> Type -> Type
substInType (v, t) = go
    where
      go (AppT x y) = AppT (go x) (go y)
      go s@(ConT _) = s
      go s@(VarT w) | v == w = t
                    | otherwise = s
      go ArrowT = ArrowT
      go ListT = ListT
      go s@(ForallT vars cxt body) 
          | v `elem` vars = s -- shadowed
          | otherwise = 
              let
                  -- prevent capture
                  freshes = evades vars t
                  substs = zip vars (VarT <$> freshes)
                  doSubsts x = foldr substInType x substs
                               
              in
                ForallT freshes (fmap (go . doSubsts) cxt ) 
                                (     (go . doSubsts) body)
                        
      go s@(TupleT _) = s

-- testCapture :: Type
-- testCapture = 
--     let 
--         n = mkName
--         v = VarT . mkName
--     in
--       substInType (n "x", v "y" `AppT` v "z")
--                   (ForallT 
--                    [n "y",n "z"] 
--                    [ConT (mkName "Show") `AppT` v "x" `AppT` v "z"]
--                    (v "x" `AppT` v "y"))

                        
-- | Make a list of names (based on the first arg) such that every name in the result
-- is distinct from every name in the second arg, and from the other results
evades :: (Data t) => [Name] -> t -> [Name]
evades ns t = foldr c [] ns
    where
      c n rec = evade n (rec,t) : rec

        

-- | Make a name (based on the first arg) that's distinct from every name in the second arg
evade :: Data d => Name -> d -> Name
evade n t = 
    let
        vars :: Set Name
        vars = everything union (mkQ Set.empty Set.singleton) t

        go n1 = if n1 `member` vars
                then go (bump n1)
                else n1
                     
        bump = mkName . ('f':) . nameBase
    in
      go n

-- evadeTest = let v = mkName "x"
--             in
--               evade v (AppT (VarT v) (VarT (mkName "fx")))
