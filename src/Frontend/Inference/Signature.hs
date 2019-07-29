{- |
Module      :  Frontend.Inference.Signature
Description :  Definition of signatures
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with the definition of signatures of sorts, kinds and types
-}
module Frontend.Inference.Signature
    ( Sort(..)
    , SortSubstitutable(..)
    , Kind(..)
    , KindParams
    , KindSubstitutable(..)
    , Type(..)
    , TypeParams
    , TypeSubstitutable(..)
    , WithSort(..)
    , WithKindParams(..)
    , WithKind(..)
    , WithTypeParams(..)
    , WithType(..)
    , WithContext(..)
    , getFullSort
    , getFullKind
    , SortSignature(..)
    , createSortSignature
    , KindConstructorSignature(..)
    , createKindConstructorSignature
    , KindSignature(..)
    , createKindSignature
    , TypeConstructorSignature(..)
    , createTypeConstructorSignature
    , TypeSignature(..)
    , createTypeSignature
    , Constraint(..)
    ) where

import Data.Bifunctor (second)
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)

import Frontend.Desugaring.Final.Ast (Ident)
import Frontend.Inference.Constraint
import Frontend.Inference.Kind
import Frontend.Inference.Sort
import Frontend.Inference.Substitution
import Frontend.Inference.Expression
import Frontend.Inference.Type

-- | A class of types which support substitution of sort variables with sorts
class SortSubstitutable a where
    substituteSort :: Substitution Sort -> a -> a -- ^ Substitute sort variables

-- | A list of kind parameters
type KindParams = [(Ident, Sort)]

-- | A class of types which support substitution of kind variables with kinds
class KindSubstitutable a where
    substituteKind :: Substitution Kind -> KindParams -> a -> a -- ^ Substitute kind variables

-- | A list of type parameters
type TypeParams = [(Ident, Kind)]

-- | A class of types which support substitution of type variables with types
class TypeSubstitutable a where
    substituteType :: Substitution Type -> TypeParams -> a -> a -- ^ Substitute type variables

-- | A class of types which have a sort
class WithSort a where
    getSort :: a -> Sort -- ^ Get a sort of an object

-- | A class of types which have kind parameters
class WithKindParams a where
    getKindParams :: a -> KindParams -- ^ Get kind parameters of an object

-- | A class of types which have a kind
class WithKind a where
    getKind :: a -> Kind -- ^ Get a kind of an object

-- | A class of types which have type parameters
class WithTypeParams a where
    getTypeParams :: a -> TypeParams -- ^ Get type parameters of an object

-- | A class of types which have a type
class WithType a where
    getType :: a -> Type -- ^ Get a type of an object

-- | A class of types which have a context
class WithContext a where
    getContext :: a -> [Constraint] -- ^ Get a context of an object

-- | Get a full sort of an object, including it's parametrs
getFullSort :: (WithSort a, WithKindParams a) => a -> Sort
getFullSort a = foldr (SortFunction . snd) (getSort a) (getKindParams a)

-- | Get a full kind of an object, including it's parameters
getFullKind :: (WithKind a, WithTypeParams a) => a -> Kind
getFullKind a = foldr (KindFunction . snd) (getKind a) (getTypeParams a)

-- | A signature of a sort
newtype SortSignature = SortSignature
    { getSortSignatureSort :: Sort -- ^ A sort
    } deriving (Eq, Show)

instance WithSort SortSignature where
    getSort = getSortSignatureSort

instance SortSubstitutable SortSignature where
    substituteSort sub SortSignature {getSortSignatureSort = sort} =
        SortSignature {getSortSignatureSort = substitute sub sort}

-- | Creates a sort signature
createSortSignature :: (WithSort a) => a -> SortSignature
createSortSignature s = SortSignature {getSortSignatureSort = getSort s}

-- | A signature of a kind constructor
data KindConstructorSignature = KindConstructorSignature
    { getKindConstructorSignatureSort :: Sort -- ^ A resulting sort of the kind constructor
    , getKindConstructorSignatureKindParams :: KindParams -- ^ Parameters of the kind constructor
    } deriving (Eq, Show)

instance WithSort KindConstructorSignature where
    getSort = getKindConstructorSignatureSort

instance WithKindParams KindConstructorSignature where
    getKindParams = getKindConstructorSignatureKindParams

instance SortSubstitutable KindConstructorSignature where
    substituteSort sub KindConstructorSignature { getKindConstructorSignatureSort = sort
                                                , getKindConstructorSignatureKindParams = kindParams
                                                } =
        KindConstructorSignature
            { getKindConstructorSignatureSort = substitute sub sort
            , getKindConstructorSignatureKindParams =
                  map (second $ substitute sub) kindParams
            }

-- | Creates a kind constructor signature
createKindConstructorSignature ::
       (WithSort a, WithKindParams a) => a -> KindConstructorSignature
createKindConstructorSignature s =
    KindConstructorSignature
        { getKindConstructorSignatureSort = getSort s
        , getKindConstructorSignatureKindParams = getKindParams s
        }

-- | A signature of a kind
data KindSignature = KindSignature
    { getKindSignatureSort :: Sort -- ^ A resulting sort of the kind
    , getKindSignatureKindParams :: KindParams -- ^ Parameters of the kind
    , getKindSignatureKind :: Kind -- ^ A kind
    } deriving (Eq, Show)

instance WithSort KindSignature where
    getSort = getKindSignatureSort

instance WithKindParams KindSignature where
    getKindParams = getKindSignatureKindParams

instance WithKind KindSignature where
    getKind = getKindSignatureKind

instance SortSubstitutable KindSignature where
    substituteSort sub sig@KindSignature { getKindSignatureSort = sort
                                         , getKindSignatureKindParams = kindParams
                                         } =
        sig
            { getKindSignatureSort = substitute sub sort
            , getKindSignatureKindParams =
                  map (second $ substitute sub) kindParams
            }

instance KindSubstitutable KindSignature where
    substituteKind sub sorts sig@KindSignature { getKindSignatureKindParams = kindParams
                                               , getKindSignatureKind = kind
                                               } =
        let substitutedKind = substitute sub kind
            freeVars = getFreeVariables substitutedKind
         in sig
                { getKindSignatureKindParams =
                      filterParams freeVars sorts kindParams
                , getKindSignatureKind = substitutedKind
                }

-- | Creates a kind signature
createKindSignature ::
       (WithSort a, WithKindParams a, WithKind a) => a -> KindSignature
createKindSignature s =
    KindSignature
        { getKindSignatureSort = getSort s
        , getKindSignatureKindParams = getKindParams s
        , getKindSignatureKind = getKind s
        }

-- | A signature of a type constructor
data TypeConstructorSignature = TypeConstructorSignature
    { getTypeConstructorSignatureSort :: Sort -- ^ A resulting sort of the type constructor
    , getTypeConstructorSignatureKindParams :: KindParams -- ^ Kind parameters of the type constructor
    , getTypeConstructorSignatureKind :: Kind -- ^ A resulting kind of the type constructor
    , getTypeConstructorSignatureTypeParams :: TypeParams -- ^ Type parameters of the type constructor
    } deriving (Eq, Show)

instance WithSort TypeConstructorSignature where
    getSort = getTypeConstructorSignatureSort

instance WithKindParams TypeConstructorSignature where
    getKindParams = getTypeConstructorSignatureKindParams

instance WithKind TypeConstructorSignature where
    getKind = getTypeConstructorSignatureKind

instance WithTypeParams TypeConstructorSignature where
    getTypeParams = getTypeConstructorSignatureTypeParams

instance SortSubstitutable TypeConstructorSignature where
    substituteSort sub sig@TypeConstructorSignature { getTypeConstructorSignatureSort = sort
                                                    , getTypeConstructorSignatureKindParams = kindParams
                                                    } =
        sig
            { getTypeConstructorSignatureSort = substitute sub sort
            , getTypeConstructorSignatureKindParams =
                  map (second $ substitute sub) kindParams
            }

instance KindSubstitutable TypeConstructorSignature where
    substituteKind sub sorts sig@TypeConstructorSignature { getTypeConstructorSignatureKindParams = kindParams
                                                          , getTypeConstructorSignatureKind = kind
                                                          , getTypeConstructorSignatureTypeParams = typeParams
                                                          } =
        let substitutedKind = substitute sub kind
            substitutedTypeParams = map (second $ substitute sub) typeParams
            freeVars =
                HS.unions . map getFreeVariables $
                substitutedKind : map snd substitutedTypeParams
         in sig
                { getTypeConstructorSignatureKindParams =
                      filterParams freeVars sorts kindParams
                , getTypeConstructorSignatureKind = substitutedKind
                , getTypeConstructorSignatureTypeParams = substitutedTypeParams
                }

-- | Creates a type constructor signature
createTypeConstructorSignature ::
       (WithSort a, WithKindParams a, WithKind a, WithTypeParams a)
    => a
    -> TypeConstructorSignature
createTypeConstructorSignature s =
    TypeConstructorSignature
        { getTypeConstructorSignatureSort = getSort s
        , getTypeConstructorSignatureKindParams = getKindParams s
        , getTypeConstructorSignatureKind = getKind s
        , getTypeConstructorSignatureTypeParams = getTypeParams s
        }

-- | A signature of a type
data TypeSignature = TypeSignature
    { getTypeSignatureSort :: Sort -- ^ A resulting sort of the type
    , getTypeSignatureKindParams :: KindParams -- ^ Kind parameters of the type
    , getTypeSignatureKind :: Kind -- ^ A resulting kind
    , getTypeSignatureTypeParams :: TypeParams -- ^ Type parameters of the type
    , getTypeSignatureType :: Type -- ^ A type
    , getTypeSignatureContext :: [Constraint] -- ^ Context of a type
    } deriving (Eq, Show)

instance WithSort TypeSignature where
    getSort = getTypeSignatureSort

instance WithKindParams TypeSignature where
    getKindParams = getTypeSignatureKindParams

instance WithKind TypeSignature where
    getKind = getTypeSignatureKind

instance WithTypeParams TypeSignature where
    getTypeParams = getTypeSignatureTypeParams

instance WithType TypeSignature where
    getType = getTypeSignatureType

instance WithContext TypeSignature where
    getContext = getTypeSignatureContext

instance SortSubstitutable TypeSignature where
    substituteSort sub sig@TypeSignature { getTypeSignatureSort = sort
                                         , getTypeSignatureKindParams = kindParams
                                         } =
        sig
            { getTypeSignatureSort = substitute sub sort
            , getTypeSignatureKindParams =
                  map (second $ substitute sub) kindParams
            }

instance KindSubstitutable TypeSignature where
    substituteKind sub sorts sig@TypeSignature { getTypeSignatureKindParams = kindParams
                                               , getTypeSignatureKind = kind
                                               , getTypeSignatureTypeParams = typeParams
                                               } =
        let substitutedKind = substitute sub kind
            substitutedParams = map (second $ substitute sub) typeParams
            freeVars =
                HS.unions . map getFreeVariables $
                substitutedKind : map snd substitutedParams
         in sig
                { getTypeSignatureKindParams =
                      filterParams freeVars sorts kindParams
                , getTypeSignatureKind = substitutedKind
                , getTypeSignatureTypeParams = substitutedParams
                }

instance TypeSubstitutable TypeSignature where
    substituteType sub kinds sig@TypeSignature { getTypeSignatureTypeParams = typeParams
                                               , getTypeSignatureType = type'
                                               , getTypeSignatureContext = context
                                               } =
        let substitutedType = substitute sub type'
            substitutedContext = map (substituteType sub kinds) context
            freeVars =
                HS.unions $
                getFreeVariables substitutedType :
                map getFreeVariables substitutedContext
         in sig
                { getTypeSignatureTypeParams =
                      filterParams freeVars kinds typeParams
                , getTypeSignatureType = substitutedType
                , getTypeSignatureContext = substitutedContext
                }

-- | Creates a type signature
createTypeSignature ::
       ( WithSort a
       , WithKindParams a
       , WithKind a
       , WithTypeParams a
       , WithTypeParams a
       , WithType a
       , WithContext a
       )
    => a
    -> TypeSignature
createTypeSignature s =
    TypeSignature
        { getTypeSignatureSort = getSort s
        , getTypeSignatureKindParams = getKindParams s
        , getTypeSignatureKind = getKind s
        , getTypeSignatureTypeParams = getTypeParams s
        , getTypeSignatureType = getType s
        , getTypeSignatureContext = getContext s
        }

instance TypeSubstitutable Constraint where
    substituteType sub _ constr =
        case constr of
            ConstraintVariable cls var ->
                ConstraintVariable cls (substitute sub var)
            ConstraintType cls type' args ->
                ConstraintType cls type' (fmap (substitute sub) args)

-- | Select only free variables, and add missing ones
filterParams :: HS.HashSet Ident -> [(Ident, a)] -> [(Ident, a)] -> [(Ident, a)]
filterParams freeVars missing params =
    let existingVars = HS.fromList $ map fst params
        stillFreeVars = filter (\(k, _) -> k `HS.member` freeVars) params
        newVars =
            map (\var -> (var, fromJust $ lookup var missing)) . HS.toList $
            freeVars `HS.difference` existingVars
     in stillFreeVars ++ newVars
