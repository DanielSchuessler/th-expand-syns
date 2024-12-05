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

import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Datatype.TyVarBndr
import Language.Haskell.TH hiding(cxt)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Semigroup as Sem
import qualified Data.Set as Set
import Data.Generics
import Control.Monad
import Prelude

packagename :: String
packagename = "th-expand-syns"

tyVarBndrSetName :: Name -> TyVarBndr_ flag -> TyVarBndr_ flag
tyVarBndrSetName n = mapTVName (const n)

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
warn msg = reportWarning (packagename ++": WARNING: "++msg)

warnIfNameIsTypeFamily :: Name -> Q ()
warnIfNameIsTypeFamily n = do
  i <- reify n
  case i of
    ClassI {} -> return ()
    ClassOpI {} -> return ()
    TyConI d -> warnIfDecIsTypeFamily d
    FamilyI d _ -> warnIfDecIsTypeFamily d -- Called for warnings
    PrimTyConI {} -> return ()
    DataConI {} -> return ()
    VarI {} -> return ()
    TyVarI {} -> return ()
#if MIN_VERSION_template_haskell(2,12,0)
    PatSynI {} -> return ()
#endif

warnIfDecIsTypeFamily :: Dec -> Q ()
warnIfDecIsTypeFamily = go
  where
    go (TySynD {}) = return ()
    go (OpenTypeFamilyD (TypeFamilyHead name _ _ _)) = maybeWarnTypeFamily name
    go (ClosedTypeFamilyD (TypeFamilyHead name _ _ _) _) = maybeWarnTypeFamily name
    go (FunD {}) = return ()
    go (ValD {}) = return ()
    go (DataD {}) = return ()
    go (NewtypeD {}) = return ()
    go (ClassD {}) = return ()
    go (InstanceD {}) = return ()
    go (SigD {}) = return ()
    go (ForeignD {}) = return ()
    go (InfixD {}) = return ()
    go (PragmaD {}) = return ()
    -- Nothing to expand for data families, so no warning
    go (DataFamilyD {}) = return ()
    go (DataInstD {}) = return ()
    go (NewtypeInstD {}) = return ()
    go (TySynInstD {}) = return ()
    go (RoleAnnotD {}) = return ()
    go (StandaloneDerivD {}) = return ()
    go (DefaultSigD {}) = return ()

#if MIN_VERSION_template_haskell(2,12,0)
    go (PatSynD {}) = return ()
    go (PatSynSigD {}) = return ()
#endif

#if MIN_VERSION_template_haskell(2,15,0)
    go (ImplicitParamBindD {}) = return ()
#endif

#if MIN_VERSION_template_haskell(2,16,0)
    go (KiSigD {}) = return ()
#endif

#if MIN_VERSION_template_haskell(2,19,0)
    go (DefaultD {}) = return ()
#endif

#if MIN_VERSION_template_haskell(2,20,0)
    go (TypeDataD {}) = return ()
#endif

warnTypeFamiliesInType :: Type -> Q ()
warnTypeFamiliesInType = go
  where
    go :: Type -> Q ()
    go (ConT n)     = warnIfNameIsTypeFamily n
    go (AppT t1 t2) = go t1 >> go t2
    go (SigT t k)   = go t  >> go k
    go ListT{}      = return ()
    go ArrowT{}     = return ()
    go VarT{}       = return ()
    go TupleT{}     = return ()
    go (ForallT tvbs ctxt body) = do
      mapM_ (go . tvKind) tvbs
      mapM_ go ctxt
      go body
    go UnboxedTupleT{} = return ()
    go PromotedT{}      = return ()
    go PromotedTupleT{} = return ()
    go PromotedConsT{}  = return ()
    go PromotedNilT{}   = return ()
    go StarT{}          = return ()
    go ConstraintT{}    = return ()
    go LitT{}           = return ()
    go EqualityT{} = return ()
    go (InfixT t1 n t2) = do
      warnIfNameIsTypeFamily n
      go t1
      go t2
    go (UInfixT t1 n t2) = do
      warnIfNameIsTypeFamily n
      go t1
      go t2
    go (ParensT t) = go t
    go WildCardT{} = return ()
#if MIN_VERSION_template_haskell(2,12,0)
    go UnboxedSumT{} = return ()
#endif
#if MIN_VERSION_template_haskell(2,15,0)
    go (AppKindT t k)       = go t >> go k
    go (ImplicitParamT _ t) = go t
#endif
#if MIN_VERSION_template_haskell(2,16,0)
    go (ForallVisT tvbs body) = do
      mapM_ (go . tvKind) tvbs
      go body
#endif
#if MIN_VERSION_template_haskell(2,17,0)
    go MulArrowT{} = return ()
#endif
#if MIN_VERSION_template_haskell(2,19,0)
    go (PromotedInfixT t1 n t2) = do
      warnIfNameIsTypeFamily n
      go t1
      go t2
    go (PromotedUInfixT t1 n t2) = do
      warnIfNameIsTypeFamily n
      go t1
      go t2
#endif

maybeWarnTypeFamily :: Name -> Q ()
maybeWarnTypeFamily name =
  warn ("Type synonym families (and associated type synonyms) are currently not supported (they won't be expanded). Name of unsupported family: "++show name)

-- | Calls 'expandSynsWith' with the default settings.
expandSyns :: Type -> Q Type
expandSyns = expandSynsWith mempty

-- | Expands all type synonyms in the given type. Type families currently won't be expanded (but will be passed through).
expandSynsWith :: SynonymExpansionSettings -> Type -> Q Type
expandSynsWith settings = expandSyns'
    where
      expandSyns' x = do
        when (sesWarnTypeFamilies settings) $
          warnTypeFamiliesInType x
        resolveTypeSynonyms x

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

-- | Capture-free substitution
substInType :: (Name,Type) -> Type -> Type
substInType vt = applySubstitution (Map.fromList [vt])

-- | Capture-free substitution
substInCon :: (Name,Type) -> Con -> Con
substInCon vt = go
    where
      vtSubst = Map.fromList [vt]
      st = applySubstitution vtSubst

      go (NormalC n ts) = NormalC n [(x, st y) | (x,y) <- ts]
      go (RecC n ts) = RecC n [(x, y, st z) | (x,y,z) <- ts]
      go (InfixC (y1,t1) op (y2,t2)) = InfixC (y1,st t1) op (y2,st t2)
      go (ForallC vars cxt body) =
          commonForallCase vt vars $ \vts' vars' ->
          ForallC (map (mapTVKind (applySubstitution vts')) vars')
                  (applySubstitution vts' cxt)
                  (Map.foldrWithKey (\v t -> substInCon (v, t)) body vts')
      go c@GadtC{} = errGadt c
      go c@RecGadtC{} = errGadt c

      errGadt c = error (packagename++": substInCon currently doesn't support GADT constructors with GHC >= 8 ("++pprint c++")")

-- Apply a substitution to something underneath a @forall@. The continuation
-- argument provides new substitutions and fresh type variable binders to avoid
-- the outer substitution from capturing the thing underneath the @forall@.
commonForallCase :: (Name, Type) -> [TyVarBndr_ flag]
                 -> (Map Name Type -> [TyVarBndr_ flag] -> a)
                 -> a
commonForallCase vt@(v,t) bndrs k
            -- If a variable with the same name as the one to be replaced is bound by the forall,
            -- the variable to be replaced is shadowed in the body, so we leave the whole thing alone (no recursion)
          | v `elem` (tvName <$> bndrs) = k (Map.fromList [vt]) bndrs

          | otherwise =
              let
                  -- prevent capture
                  vars = tvName <$> bndrs
                  freshes = evades vars t
                  freshTyVarBndrs = zipWith tyVarBndrSetName freshes bndrs
                  substs = zip vars (VarT <$> freshes)
              in
                k (Map.fromList (vt:substs)) freshTyVarBndrs
