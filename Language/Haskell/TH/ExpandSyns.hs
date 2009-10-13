{-# OPTIONS -Wall -fno-warn-unused-binds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Haskell.TH.ExpandSyns(-- * Expand synonyms
                                      expandSyns
                                      -- * Misc utilities
                                     ,substInType, substInCon
                                     ,evades,evade) where
    
import Language.Haskell.TH hiding(cxt)
import Data.Set as Set    
import Data.Generics
import Control.Monad
    
    
-- Compatibility layer for TH 2.4 vs. 2.3
tyVarBndrGetName :: TyVarBndr -> Name
mapPred :: (Type -> Type) -> Pred -> Pred
bindPred :: (Type -> Q Type) -> Pred -> Q Pred
tyVarBndrSetName :: Name -> TyVarBndr -> TyVarBndr
                   
#if __GLASGOW_HASKELL__ >= 611
tyVarBndrGetName (PlainTV n) = n
tyVarBndrGetName (KindedTV n _) = n
                                
mapPred f (ClassP n ts) = ClassP n (f <$> ts)
mapPred f (EqualP t1 t2) = EqualP (f t1) (f t2)
                            
bindPred f (ClassP n ts) = ClassP n <$> mapM f ts
bindPred f (EqualP t1 t2) = EqualP <$> f t1 <*> f t2
                            
tyVarBndrSetName n (PlainTV _) = PlainTV n
tyVarBndrSetName n (KindedTV _ k) = KindedTV n k 
#else

type TyVarBndr = Name
type Pred = Type
tyVarBndrGetName = id
mapPred = id
bindPred = id
tyVarBndrSetName n _ = n
                       
#endif

substInPred :: (Name, Type) -> Pred -> Pred
substInPred s = mapPred (substInType s)


(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap
(<*>) :: (Monad m) => m (a -> b) -> m a -> m b
(<*>) = ap

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
decIsSyn (TySynD _ vars t) = Just (tyVarBndrGetName <$> vars,t)
decIsSyn x = error ("decIsSyn: unexpected dec: "++show x)

-- | Expands type synonyms...
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
        cxt' <- mapM (bindPred expandSyns) cxt
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
                        

#if __GLASGOW_HASKELL__ >= 611
      go acc (SigT t kind) = 
          do
            (acc',t') <- go acc t
            return (acc',SigT t' kind)
#endif

-- | Capture-free substitution
substInType :: (Name, Type) -> Type -> Type
substInType (v, t) = go
    where
      go (AppT x y) = AppT (go x) (go y)
      go s@(ConT _) = s
      go s@(VarT w) | v == w = t
                    | otherwise = s
      go ArrowT = ArrowT
      go ListT = ListT
      go (ForallT vars cxt body) = 
          commonForallCase (v,t) ForallT substInType (vars,cxt,body)
                        
      go s@(TupleT _) = s
                        
#if __GLASGOW_HASKELL__ >= 611
      go (SigT t1 kind) = SigT (go t1) kind
#endif

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
         
-- | Make a list of names (based on the first arg) such that every name in the result
-- is distinct from every name in the second arg, and from the other results
evades :: (Data t) => [Name] -> t -> [Name]
evades ns t = foldr c [] ns
    where
      c n rec = evade n (rec,t) : rec

-- evadeTest = let v = mkName "x"
--             in
--               evade v (AppT (VarT v) (VarT (mkName "fx")))

substInCon :: (Name, Type) -> Con -> Con
substInCon (v,t) = go
    where
      st = substInType (v,t)

      go (NormalC n ts) = NormalC n [(x, st y) | (x,y) <- ts]
      go (RecC n ts) = RecC n [(x, y, st z) | (x,y,z) <- ts]
      go (InfixC (y1,t1) op (y2,t2)) = InfixC (y1,st t1) op (y2,st t2)
      go (ForallC vars cxt body) = 
          commonForallCase (v,t) ForallC substInCon (vars,cxt,body)






commonForallCase :: (Name,Type) 
                 -> ([TyVarBndr] -> Cxt -> a -> a) 
                 -> ((Name,Type) -> a -> a)
                 -> ([TyVarBndr],Cxt,a)
                 -> a
commonForallCase (v,t) forallCon bodySubst (bndrs,cxt,body)
          | v `elem` (tyVarBndrGetName <$> bndrs) = forallCon bndrs cxt body -- shadowed
          | otherwise = 
              let
                  -- prevent capture
                  vars = tyVarBndrGetName <$> bndrs
                  freshes = evades vars t
                  freshTyVarBndrs = zipWith tyVarBndrSetName freshes bndrs
                  substs = zip vars (VarT <$> freshes)
                  doSubsts f x = foldr f x substs
                               
              in
                forallCon 
                  freshTyVarBndrs
                  (fmap (substInPred (v,t) . doSubsts substInPred) cxt ) 
                  (     (  bodySubst (v,t) . doSubsts bodySubst  ) body)
