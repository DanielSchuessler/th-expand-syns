{-# OPTIONS -Wall -fno-warn-unused-binds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Haskell.TH.ExpandSyns(-- * Expand synonyms
                                      expandSyns
                                      -- * Misc utilities
                                     ,substInType
                                     ,substInCon
                                     ,evades,evade) where

import Language.Haskell.TH hiding(cxt)
import qualified Data.Set as Set
import Data.Generics
import Control.Monad

-- For ghci
#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(X,Y,Z) 1
#endif

packagename :: String
packagename = "th-expand-syns"


-- Compatibility layer for TH >=2.4 vs. 2.3
tyVarBndrGetName :: TyVarBndr -> Name
#if !MIN_VERSION_template_haskell(2,10,0)
mapPred :: (Type -> Type) -> Pred -> Pred
#endif
bindPred :: (Type -> Q Type) -> Pred -> Q Pred
tyVarBndrSetName :: Name -> TyVarBndr -> TyVarBndr

#if MIN_VERSION_template_haskell(2,4,0)
tyVarBndrGetName (PlainTV n) = n
tyVarBndrGetName (KindedTV n _) = n

#if MIN_VERSION_template_haskell(2,10,0)
bindPred = id
#else
mapPred f (ClassP n ts) = ClassP n (f <$> ts)
mapPred f (EqualP t1 t2) = EqualP (f t1) (f t2)

bindPred f (ClassP n ts) = ClassP n <$> mapM f ts
bindPred f (EqualP t1 t2) = EqualP <$> f t1 <*> f t2
#endif

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


#if __GLASGOW_HASKELL__ < 709
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap
#endif
(<*>) :: (Monad m) => m (a -> b) -> m a -> m b
(<*>) = ap

type SynInfo = ([Name],Type)

nameIsSyn :: Name -> Q (Maybe SynInfo)
nameIsSyn n = do
  i <- reify n
  case i of
    TyConI d -> decIsSyn d
    ClassI {} -> return Nothing
    PrimTyConI {} -> return Nothing
#if MIN_VERSION_template_haskell(2,7,0)
    FamilyI (FamilyD flavour name _ _) _ -> maybeWarnTypeFamily flavour name >> return Nothing
#endif
    _ -> do
            warn ("Don't know how to interpret the result of reify "++show n++" (= "++show i++").\n"++
                  "I will assume that "++show n++" is not a type synonym.")
            return Nothing



warn ::  String -> Q ()
warn msg =
#if MIN_VERSION_template_haskell(2,8,0)
    reportWarning
#else
    report False
#endif
      (packagename ++": "++"WARNING: "++msg)


#if MIN_VERSION_template_haskell(2,4,0)
maybeWarnTypeFamily :: FamFlavour -> Name -> Q ()
maybeWarnTypeFamily flavour name =
  case flavour of
    TypeFam ->
      warn ("Type synonym families (and associated type synonyms) are currently not supported (they won't be expanded). Name of unsupported family: "++show name)

    DataFam -> return ()
      -- Nothing to expand for data families, so no warning
#endif

-- | Handles only declaration constructs that can be returned by 'reify'ing a type name.
decIsSyn :: Dec -> Q (Maybe SynInfo)
decIsSyn (ClassD {}) = return Nothing
decIsSyn (DataD {}) = return Nothing
decIsSyn (NewtypeD {}) = return Nothing
decIsSyn (TySynD _ vars t) = return (Just (tyVarBndrGetName <$> vars,t))
#if MIN_VERSION_template_haskell(2,4,0)
decIsSyn (FamilyD flavour name _ _) = maybeWarnTypeFamily flavour name >> return Nothing
#endif
decIsSyn x = do
    warn ("Unrecognized declaration construct: "++ show x++". I will assume that it's not a type synonym declaration.")
    return Nothing





-- | Expands all type synonyms in the given type. Type families currently won't be expanded (but will be passed through).
expandSyns :: Type -> Q Type
expandSyns = \t ->
         do
           (acc,t') <- go [] t
           return (foldl AppT t' acc)


    where
      -- Must only be called on an `x' requiring no expansion
      passThrough acc x = return (acc, x)

      -- If @go args t = (args', t')@,
      --
      -- Precondition:
      --  All elements of `args' are expanded.
      -- Postcondition:
      --  All elements of `args'' and `t'' are expanded.
      --  `t' applied to `args' equals `t'' applied to `args'' (up to expansion, of course)

      go :: [Type] -> Type -> Q ([Type], Type)

      go acc x@ListT = passThrough acc x
      go acc x@ArrowT = passThrough acc x
      go acc x@(TupleT _) = passThrough acc x
      go acc x@(VarT _) = passThrough acc x

      go [] (ForallT ns cxt t) = do
        cxt' <- mapM (bindPred expandSyns) cxt
        t' <- expandSyns t
        return ([], ForallT ns cxt' t')

      go acc x@(ForallT _ _ _) =
          fail (packagename++": Unexpected application of the local quantification: "
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
              Nothing -> return (acc, x)
              Just (vars,body) ->
                  if length acc < length vars
                  then fail (packagename++": expandSyns: Underapplied type synonym: "++show(n,acc))
                  else
                      let
                          substs = zip vars acc
                          expanded = foldr subst body substs
                      in
                        go (drop (length vars) acc) expanded


#if MIN_VERSION_template_haskell(2,4,0)
      go acc (SigT t kind) =
          do
            (acc',t') <- go acc t
            return
              (acc',
                SigT t' kind
                -- No expansion needed in kinds (todo: is this correct?)
              )
#endif

#if MIN_VERSION_template_haskell(2,6,0)
      go acc x@(UnboxedTupleT _) = passThrough acc x
#endif

#if MIN_VERSION_template_haskell(2,8,0)
      go acc x@(PromotedT _) = passThrough acc x
      go acc x@(PromotedTupleT _) = passThrough acc x
      go acc x@PromotedConsT = passThrough acc x
      go acc x@PromotedNilT = passThrough acc x
      go acc x@StarT = passThrough acc x
      go acc x@ConstraintT = passThrough acc x
      go acc x@(LitT _) = passThrough acc x
#endif

#if MIN_VERSION_template_haskell(2,10,0)
      go acc x@EqualityT = passThrough acc x
#endif

class SubstTypeVariable a where
    -- | Capture-free substitution
    subst :: (Name, Type) -> a -> a



instance SubstTypeVariable Type where
  subst (v, t) = go
    where
      go (AppT x y) = AppT (go x) (go y)
      go s@(ConT _) = s
      go s@(VarT w) | v == w = t
                    | otherwise = s
      go ArrowT = ArrowT
      go ListT = ListT
      go (ForallT vars cxt body) =
          commonForallCase (v,t) (vars,cxt,body)

      go s@(TupleT _) = s

#if MIN_VERSION_template_haskell(2,4,0)
      go (SigT t1 kind) = SigT (go t1) kind
#endif

#if MIN_VERSION_template_haskell(2,6,0)
      go s@(UnboxedTupleT _) = s
#endif

#if MIN_VERSION_template_haskell(2,8,0)
      go s@(PromotedT _) = s
      go s@(PromotedTupleT _) = s
      go s@PromotedConsT = s
      go s@PromotedNilT = s
      go s@StarT = s
      go s@ConstraintT = s
      go s@(LitT _) = s
#endif

#if MIN_VERSION_template_haskell(2,10,0)
      go s@EqualityT = s
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


#if MIN_VERSION_template_haskell(2,4,0) && !MIN_VERSION_template_haskell(2,10,0)
instance SubstTypeVariable Pred where
    subst s = mapPred (subst s)
#endif


-- | Make a name (based on the first arg) that's distinct from every name in the second arg
--
-- Example why this is necessary:
--
-- > type E x = forall y. Either x y
-- >
-- > ... expandSyns [t| forall y. y -> E y |]
--
-- The example as given may actually work correctly without any special capture-avoidance depending
-- on how GHC handles the @y@s, but in any case, the input type to expandSyns may be an explicit
-- AST using 'mkName' to ensure a collision.
--
evade :: Data d => Name -> d -> Name
evade n t =
    let
        vars :: Set.Set Name
        vars = everything Set.union (mkQ Set.empty Set.singleton) t

        go n1 = if n1 `Set.member` vars
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

instance SubstTypeVariable Con where
  subst (v,t) = go
    where
      st = subst (v,t)

      go (NormalC n ts) = NormalC n [(x, st y) | (x,y) <- ts]
      go (RecC n ts) = RecC n [(x, y, st z) | (x,y,z) <- ts]
      go (InfixC (y1,t1) op (y2,t2)) = InfixC (y1,st t1) op (y2,st t2)
      go (ForallC vars cxt body) =
          commonForallCase (v,t) (vars,cxt,body)



class HasForallConstruct a where
    mkForall :: [TyVarBndr] -> Cxt -> a -> a

instance HasForallConstruct Type where
    mkForall = ForallT

instance HasForallConstruct Con where
    mkForall = ForallC



commonForallCase :: (SubstTypeVariable a, HasForallConstruct a) =>

                    (Name,Type)
                 -> ([TyVarBndr],Cxt,a)
                 -> a
commonForallCase vt@(v,t) (bndrs,cxt,body)

            -- If a variable with the same name as the one to be replaced is bound by the forall,
            -- the variable to be replaced is shadowed in the body, so we leave the whole thing alone (no recursion)
          | v `elem` (tyVarBndrGetName <$> bndrs) = mkForall bndrs cxt body

          | otherwise =
              let
                  -- prevent capture
                  vars = tyVarBndrGetName <$> bndrs
                  freshes = evades vars t
                  freshTyVarBndrs = zipWith tyVarBndrSetName freshes bndrs
                  substs = zip vars (VarT <$> freshes)
                  doSubsts :: SubstTypeVariable b => b -> b
                  doSubsts x = foldr subst x substs

              in
                mkForall
                  freshTyVarBndrs
                  (fmap (subst vt . doSubsts) cxt )
                  (     (subst vt . doSubsts) body)



-- | Capture-free substitution
substInType :: (Name,Type) -> Type -> Type
substInType = subst

-- | Capture-free substitution
substInCon :: (Name,Type) -> Con -> Con
substInCon = subst
