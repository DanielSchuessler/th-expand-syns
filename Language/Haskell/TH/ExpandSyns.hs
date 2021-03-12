{-# OPTIONS -Wall -fno-warn-unused-binds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.Haskell.TH.ExpandSyns(-- * Expand synonyms
                                      expandSyns
                                     ,expandSynsWith
                                     ,SynonymExpansionSettings
                                     ,noWarnTypeFamilies

                                      -- * Misc utilities
                                     ,substInType
                                     ,substInCon
                                     ,evades,evade) where

import Language.Haskell.TH.ExpandSyns.SemigroupCompat as Sem
import Language.Haskell.TH hiding(cxt)
import qualified Data.Set as Set
import Data.Generics
import Data.Maybe
import Control.Monad
import Prelude

-- For ghci
#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(X,Y,Z) 1
#endif

packagename :: String
packagename = "th-expand-syns"

#if MIN_VERSION_template_haskell(2,17,0)
tyVarBndrGetName :: TyVarBndr a -> Name
tyVarBndrGetName (PlainTV n _) = n
tyVarBndrGetName (KindedTV n _ _) = n
#else
tyVarBndrGetName :: TyVarBndr -> Name
tyVarBndrGetName (PlainTV n) = n
tyVarBndrGetName (KindedTV n _) = n
#endif

#if MIN_VERSION_template_haskell(2,17,0)
tyVarBndrSetName :: Name -> TyVarBndr a -> TyVarBndr a
tyVarBndrSetName n (PlainTV _ f) = PlainTV n f
tyVarBndrSetName n (KindedTV _ f k) = KindedTV n f k
#else
tyVarBndrSetName :: Name -> TyVarBndr -> TyVarBndr
tyVarBndrSetName n (PlainTV _) = PlainTV n
tyVarBndrSetName n (KindedTV _ k) = KindedTV n k
#endif

#if MIN_VERSION_template_haskell(2,10,0)
-- mapPred is not needed for template-haskell >= 2.10
#else
mapPred :: (Type -> Type) -> Pred -> Pred
mapPred f (ClassP n ts) = ClassP n (f <$> ts)
mapPred f (EqualP t1 t2) = EqualP (f t1) (f t2)
#endif

#if MIN_VERSION_template_haskell(2,10,0)
bindPred :: (Type -> Q Type) -> Pred -> Q Pred
bindPred = id
#else
bindPred :: (Type -> Q Type) -> Pred -> Q Pred
bindPred f (ClassP n ts) = ClassP n <$> mapM f ts
bindPred f (EqualP t1 t2) = EqualP <$> f t1 <*> f t2
#endif

#if __GLASGOW_HASKELL__ < 709
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap
#endif
(<*>) :: (Monad m) => m (a -> b) -> m a -> m b
(<*>) = ap



data SynonymExpansionSettings =
  SynonymExpansionSettings {
    sesWarnTypeFamilies :: Bool
  }


instance Semigroup SynonymExpansionSettings where
  SynonymExpansionSettings w1 <> SynonymExpansionSettings w2 =
    SynonymExpansionSettings (w1 && w2)

-- | Default settings ('mempty'):
--
-- * Warn if type families are encountered.
--
-- (The 'mappend' is currently rather useless; the monoid instance is intended for additional settings in the future).
instance Monoid SynonymExpansionSettings where
  mempty =
    SynonymExpansionSettings {
      sesWarnTypeFamilies = True
    }

#if !MIN_VERSION_base(4,11,0)
-- starting with base-4.11, mappend definitions are redundant;
-- at some point `mappend` will be removed from `Monoid`
  mappend = (Sem.<>)
#endif


-- | Suppresses the warning that type families are unsupported.
noWarnTypeFamilies :: SynonymExpansionSettings
noWarnTypeFamilies = mempty { sesWarnTypeFamilies = False }

warn ::  String -> Q ()
warn msg =
#if MIN_VERSION_template_haskell(2,8,0)
    reportWarning
#else
    report False
#endif
      (packagename ++": WARNING: "++msg)




type SynInfo = ([Name],Type)

nameIsSyn :: SynonymExpansionSettings -> Name -> Q (Maybe SynInfo)
nameIsSyn settings n = do
  i <- reify n
  case i of
    ClassI {} -> no
    ClassOpI {} -> no
    TyConI d -> decIsSyn settings d
#if MIN_VERSION_template_haskell(2,7,0)
    FamilyI d _ -> decIsSyn settings d -- Called for warnings
#endif
    PrimTyConI {} -> no
    DataConI {} -> no
    VarI {} -> no
    TyVarI {} -> no
#if MIN_VERSION_template_haskell(2,12,0)
    PatSynI {} -> no
#endif

  where
    no = return Nothing

decIsSyn :: SynonymExpansionSettings -> Dec -> Q (Maybe SynInfo)
decIsSyn settings = go
  where
    go (TySynD _ vars t) = return (Just (tyVarBndrGetName <$> vars,t))

#if MIN_VERSION_template_haskell(2,11,0)
    go (OpenTypeFamilyD (TypeFamilyHead name _ _ _)) = maybeWarnTypeFamily settings name >> no
    go (ClosedTypeFamilyD (TypeFamilyHead name _ _ _) _) = maybeWarnTypeFamily settings name >> no
#else

#if MIN_VERSION_template_haskell(2,9,0)
    go (ClosedTypeFamilyD name _ _ _) = maybeWarnTypeFamily settings name >> no
#endif

    go (FamilyD TypeFam name _ _) = maybeWarnTypeFamily settings name >> no
#endif

    go (FunD {}) = no
    go (ValD {}) = no
    go (DataD {}) = no
    go (NewtypeD {}) = no
    go (ClassD {}) = no
    go (InstanceD {}) = no
    go (SigD {}) = no
    go (ForeignD {}) = no

#if MIN_VERSION_template_haskell(2,8,0)
    go (InfixD {}) = no
#endif

    go (PragmaD {}) = no

    -- Nothing to expand for data families, so no warning
#if MIN_VERSION_template_haskell(2,11,0)
    go (DataFamilyD {}) = no
#else
    go (FamilyD DataFam _ _ _) = no
#endif

    go (DataInstD {}) = no
    go (NewtypeInstD {}) = no
    go (TySynInstD {}) = no

#if MIN_VERSION_template_haskell(2,9,0)
    go (RoleAnnotD {}) = no
#endif

#if MIN_VERSION_template_haskell(2,10,0)
    go (StandaloneDerivD {}) = no
    go (DefaultSigD {}) = no
#endif

#if MIN_VERSION_template_haskell(2,12,0)
    go (PatSynD {}) = no
    go (PatSynSigD {}) = no
#endif

#if MIN_VERSION_template_haskell(2,15,0)
    go (ImplicitParamBindD {}) = no
#endif

#if MIN_VERSION_template_haskell(2,16,0)
    go (KiSigD {}) = no
#endif

    no = return Nothing

maybeWarnTypeFamily :: SynonymExpansionSettings -> Name -> Q ()
maybeWarnTypeFamily settings name =
  when (sesWarnTypeFamilies settings) $
      warn ("Type synonym families (and associated type synonyms) are currently not supported (they won't be expanded). Name of unsupported family: "++show name)







-- | Calls 'expandSynsWith' with the default settings.
expandSyns :: Type -> Q Type
expandSyns = expandSynsWith mempty


-- | Expands all type synonyms in the given type. Type families currently won't be expanded (but will be passed through).
expandSynsWith :: SynonymExpansionSettings -> Type -> Q Type
expandSynsWith settings = expandSyns'

    where
      expandSyns' t =
         do
           (acc,t') <- go [] t
           return (foldl applyTypeArg t' acc)

      expandKindSyns' k =
#if MIN_VERSION_template_haskell(2,8,0)
         do
           (acc,k') <- go [] k
           return (foldl applyTypeArg k' acc)
#else
         return k -- No kind variables on old versions of GHC
#endif

      applyTypeArg :: Type -> TypeArg -> Type
      applyTypeArg f (TANormal x) = f `AppT` x
      applyTypeArg f (TyArg _x)   =
#if __GLASGOW_HASKELL__ >= 807
                                    f `AppKindT` _x
#else
                                    -- VKA isn't supported, so
                                    -- conservatively drop the argument
                                    f
#endif


      -- Filter the normal type arguments from a list of TypeArgs.
      filterTANormals :: [TypeArg] -> [Type]
      filterTANormals = mapMaybe getTANormal
        where
          getTANormal :: TypeArg -> Maybe Type
          getTANormal (TANormal t) = Just t
          getTANormal (TyArg {})   = Nothing

      -- Must only be called on an `x' requiring no expansion
      passThrough acc x = return (acc, x)

      forallAppError :: [TypeArg] -> Type -> Q a
      forallAppError acc x =
          fail (packagename++": Unexpected application of the local quantification: "
                ++show x
                ++"\n    (to the arguments "++show acc++")")

      -- If @go args t = (args', t')@,
      --
      -- Precondition:
      --  All elements of `args' are expanded.
      -- Postcondition:
      --  All elements of `args'' and `t'' are expanded.
      --  `t' applied to `args' equals `t'' applied to `args'' (up to expansion, of course)

      go :: [TypeArg] -> Type -> Q ([TypeArg], Type)

      go acc x@ListT = passThrough acc x
      go acc x@ArrowT = passThrough acc x
      go acc x@(TupleT _) = passThrough acc x
      go acc x@(VarT _) = passThrough acc x

      go [] (ForallT ns cxt t) = do
        cxt' <- mapM (bindPred expandSyns') cxt
        t' <- expandSyns' t
        return ([], ForallT ns cxt' t')

      go acc x@ForallT{} = forallAppError acc x

      go acc (AppT t1 t2) =
          do
            r <- expandSyns' t2
            go (TANormal r:acc) t1

      go acc x@(ConT n) =
          do
            i <- nameIsSyn settings n
            case i of
              Nothing -> return (acc, x)
              Just (vars,body) ->
                  if length acc < length vars
                  then fail (packagename++": expandSynsWith: Underapplied type synonym: "++show(n,acc))
                  else
                      let
                          substs = zip vars (filterTANormals acc)
                          expanded = doSubsts substs body
                      in
                        go (drop (length vars) acc) expanded


      go acc (SigT t kind) =
          do
            (acc',t') <- go acc t
            kind' <- expandKindSyns' kind
            return (acc', SigT t' kind')

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

#if MIN_VERSION_template_haskell(2,11,0)
      go acc (InfixT t1 nm t2) =
          do
            t1' <- expandSyns' t1
            t2' <- expandSyns' t2
            return (acc,InfixT t1' nm t2')
      go acc (UInfixT t1 nm t2) =
          do
            t1' <- expandSyns' t1
            t2' <- expandSyns' t2
            return (acc,UInfixT t1' nm t2')
      go acc (ParensT t) =
          do
            (acc',t') <- go acc t
            return (acc',ParensT t')
      go acc x@WildCardT = passThrough acc x
#endif

#if MIN_VERSION_template_haskell(2,12,0)
      go acc x@(UnboxedSumT _) = passThrough acc x
#endif

#if MIN_VERSION_template_haskell(2,15,0)
      go acc (AppKindT t k) =
          do
            k' <- expandKindSyns' k
            go (TyArg k':acc) t
      go acc (ImplicitParamT n t) =
          do
            (acc',t') <- go acc t
            return (acc',ImplicitParamT n t')
#endif

#if MIN_VERSION_template_haskell(2,16,0)
      go [] (ForallVisT ns t) = do
        t' <- expandSyns' t
        return ([], ForallVisT ns t')

      go acc x@ForallVisT{} = forallAppError acc x
#endif

#if MIN_VERSION_template_haskell(2,17,0)
      go acc x@MulArrowT = passThrough acc x
#endif

-- | An argument to a type, either a normal type ('TANormal') or a visible
-- kind application ('TyArg').
data TypeArg
  = TANormal Type -- Normal arguments
  | TyArg    Kind -- Visible kind applications
  deriving Show

class SubstTypeVariable a where
    -- | Capture-free substitution
    subst :: (Name, Type) -> a -> a



instance SubstTypeVariable Type where
  subst vt@(v, t) = go
    where
      go (AppT x y) = AppT (go x) (go y)
      go s@(ConT _) = s
      go s@(VarT w) | v == w = t
                    | otherwise = s
      go ArrowT = ArrowT
      go ListT = ListT
      go (ForallT vars cxt body) =
          commonForallCase vt vars $ \vts' vars' ->
          ForallT vars' (map (doSubsts vts') cxt) (doSubsts vts' body)

      go s@(TupleT _) = s

      go (SigT t1 kind) = SigT (go t1) (subst vt kind)

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

#if MIN_VERSION_template_haskell(2,11,0)
      go (InfixT t1 nm t2) = InfixT (go t1) nm (go t2)
      go (UInfixT t1 nm t2) = UInfixT (go t1) nm (go t2)
      go (ParensT t1) = ParensT (go t1)
      go s@WildCardT = s
#endif

#if MIN_VERSION_template_haskell(2,12,0)
      go s@(UnboxedSumT _) = s
#endif

#if MIN_VERSION_template_haskell(2,15,0)
      go (AppKindT ty ki) = AppKindT (go ty) (go ki)
      go (ImplicitParamT n ty) = ImplicitParamT n (go ty)
#endif

#if MIN_VERSION_template_haskell(2,16,0)
      go (ForallVisT vars body) =
          commonForallCase vt vars $ \vts' vars' ->
          ForallVisT vars' (doSubsts vts' body)
#endif

#if MIN_VERSION_template_haskell(2,17,0)
      go MulArrowT = MulArrowT
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


#if !MIN_VERSION_template_haskell(2,10,0)
instance SubstTypeVariable Pred where
    subst s = mapPred (subst s)
#endif

#if !MIN_VERSION_template_haskell(2,8,0)
instance SubstTypeVariable Kind where
    subst _ = id -- No kind variables on old versions of GHC
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
  subst vt = go
    where
      st = subst vt

      go (NormalC n ts) = NormalC n [(x, st y) | (x,y) <- ts]
      go (RecC n ts) = RecC n [(x, y, st z) | (x,y,z) <- ts]
      go (InfixC (y1,t1) op (y2,t2)) = InfixC (y1,st t1) op (y2,st t2)
      go (ForallC vars cxt body) =
          commonForallCase vt vars $ \vts' vars' ->
          ForallC vars' (map (doSubsts vts') cxt) (doSubsts vts' body)
#if MIN_VERSION_template_haskell(2,11,0)
      go c@GadtC{} = errGadt c
      go c@RecGadtC{} = errGadt c

      errGadt c = error (packagename++": substInCon currently doesn't support GADT constructors with GHC >= 8 ("++pprint c++")")
#endif


class HasForallConstruct a where
#if MIN_VERSION_template_haskell(2,17,0)
    mkForall :: [TyVarBndrSpec] -> Cxt -> a -> a
#else
    mkForall :: [TyVarBndr] -> Cxt -> a -> a
#endif

instance HasForallConstruct Type where
    mkForall = ForallT

instance HasForallConstruct Con where
    mkForall = ForallC



-- Apply a substitution to something underneath a @forall@. The continuation
-- argument provides new substitutions and fresh type variable binders to avoid
-- the outer substitution from capturing the thing underneath the @forall@.
#if MIN_VERSION_template_haskell(2,17,0)
commonForallCase :: (Name, Type) -> [TyVarBndr flag]
                 -> ([(Name, Type)] -> [TyVarBndr flag] -> a)
                 -> a
#else
commonForallCase :: (Name, Type) -> [TyVarBndr]
                 -> ([(Name, Type)] -> [TyVarBndr] -> a)
                 -> a
#endif
commonForallCase vt@(v,t) bndrs k
            -- If a variable with the same name as the one to be replaced is bound by the forall,
            -- the variable to be replaced is shadowed in the body, so we leave the whole thing alone (no recursion)
          | v `elem` (tyVarBndrGetName <$> bndrs) = k [vt] bndrs

          | otherwise =
              let
                  -- prevent capture
                  vars = tyVarBndrGetName <$> bndrs
                  freshes = evades vars t
                  freshTyVarBndrs = zipWith tyVarBndrSetName freshes bndrs
                  substs = zip vars (VarT <$> freshes)
              in
                k (vt:substs) freshTyVarBndrs

-- Apply multiple substitutions.
doSubsts :: SubstTypeVariable a => [(Name, Type)] -> a -> a
doSubsts substs x = foldr subst x substs

-- | Capture-free substitution
substInType :: (Name,Type) -> Type -> Type
substInType = subst

-- | Capture-free substitution
substInCon :: (Name,Type) -> Con -> Con
substInCon = subst
