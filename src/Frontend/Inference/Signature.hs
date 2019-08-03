{- |
Module      :  Frontend.Inference.Signature
Description :  Definition of signatures
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with the definition of signatures of sorts, kinds and types
-}
module Frontend.Inference.Signature
    ( Params
    , ParamsMap
    , Sort(..)
    , SortSubstitutable(..)
    , Kind(..)
    , KindParams
    , KindSubstitutable(..)
    , KindGeneralisable(..)
    , Type(..)
    , TypeParams
    , TypeSubstitutable(..)
    , TypeGeneralisable(..)
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
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)

import Frontend.Desugaring.Final.Ast (Ident)
import Frontend.Inference.Constraint
import Frontend.Inference.Expression
import Frontend.Inference.Kind
import Frontend.Inference.Sort
import Frontend.Inference.Substitution
import Frontend.Inference.Type

-- | A class of types which support substitution of sort variables with sorts
class SortSubstitutable a where
    substituteSort :: Substitution Sort -> a -> a -- ^ Substitute sort variables

-- | A class of types which support substitution of kind variables with kinds
class KindSubstitutable a where
    substituteKind :: Substitution Kind -> a -> a -- ^ Substitute kind variables

-- | A class of types which support substitution of type variables with types
class TypeSubstitutable a where
    substituteType :: Substitution Type -> a -> a -- ^ Substitute type variables

-- | A map of parameters
type Params a = [(Ident, a)]

-- | A hashmap of parameters
type ParamsMap a = HM.HashMap Ident a

-- | A list of kind parameters
type KindParams = Params Sort

-- | A class of types which support generalisation of kind variables
class KindGeneralisable a where
    generaliseKind :: HS.HashSet Ident -> ParamsMap Sort -> a -> a -- ^ Generalise kind variables, except for the bound ones

-- | A list of type parameters
type TypeParams = Params Kind

-- | A class of types which can be generalised
class TypeGeneralisable a where
    generaliseType :: HS.HashSet Ident -> ParamsMap Kind -> a -> a -- ^ Generalise type variables, except for the bound ones

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
    substituteKind sub sig@KindSignature { getKindSignatureKindParams = kindParams
                                         , getKindSignatureKind = kind
                                         } =
        let substitutedKind = substitute sub kind
            freeVars = getFreeVariables substitutedKind
         in sig
                { getKindSignatureKindParams =
                      removeBoundParams freeVars kindParams
                , getKindSignatureKind = substitutedKind
                }

instance KindGeneralisable KindSignature where
    generaliseKind boundVars kindMapping sig@KindSignature { getKindSignatureKindParams = kindParams
                                                           , getKindSignatureKind = kind
                                                           } =
        let freeVars = getFreeVariables kind
         in sig
                { getKindSignatureKindParams =
                      generaliseParams boundVars kindMapping freeVars kindParams
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
    substituteKind sub sig@TypeConstructorSignature { getTypeConstructorSignatureKindParams = kindParams
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
                      removeBoundParams freeVars kindParams
                , getTypeConstructorSignatureKind = substitutedKind
                , getTypeConstructorSignatureTypeParams = substitutedTypeParams
                }

instance KindGeneralisable TypeConstructorSignature where
    generaliseKind boundVars kindMapping sig@TypeConstructorSignature { getTypeConstructorSignatureKindParams = kindParams
                                                                      , getTypeConstructorSignatureKind = kind
                                                                      , getTypeConstructorSignatureTypeParams = typeParams
                                                                      } =
        let freeVars =
                HS.unions . map getFreeVariables $ kind : map snd typeParams
         in sig
                { getTypeConstructorSignatureKindParams =
                      generaliseParams boundVars kindMapping freeVars kindParams
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
    substituteKind sub sig@TypeSignature { getTypeSignatureKindParams = kindParams
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
                      removeBoundParams freeVars kindParams
                , getTypeSignatureKind = substitutedKind
                , getTypeSignatureTypeParams = substitutedParams
                }

instance KindGeneralisable TypeSignature where
    generaliseKind boundVars kindMapping sig@TypeSignature { getTypeSignatureKindParams = kindParams
                                                           , getTypeSignatureKind = kind
                                                           , getTypeSignatureTypeParams = typeParams
                                                           } =
        let freeVars =
                HS.unions . map getFreeVariables $ kind : map snd typeParams
         in sig
                { getTypeSignatureKindParams =
                      generaliseParams boundVars kindMapping freeVars kindParams
                }

instance TypeSubstitutable TypeSignature where
    substituteType sub sig@TypeSignature { getTypeSignatureTypeParams = typeParams
                                         , getTypeSignatureType = type'
                                         , getTypeSignatureContext = context
                                         } =
        let substitutedType = substitute sub type'
            substitutedContext = map (substituteType sub) context
            freeVars =
                HS.unions $
                getFreeVariables substitutedType :
                map getFreeVariables substitutedContext
         in sig
                { getTypeSignatureTypeParams =
                      removeBoundParams freeVars typeParams
                , getTypeSignatureType = substitutedType
                , getTypeSignatureContext = substitutedContext
                }

instance TypeGeneralisable TypeSignature where
    generaliseType boundVars typeMapping sig@TypeSignature { getTypeSignatureTypeParams = typeParams
                                                           , getTypeSignatureType = type'
                                                           , getTypeSignatureContext = context
                                                           } =
        let freeVars =
                HS.unions $
                getFreeVariables type' : map getFreeVariables context
         in sig
                { getTypeSignatureTypeParams =
                      generaliseParams boundVars typeMapping freeVars typeParams
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
    substituteType sub constr =
        case constr of
            ConstraintVariable cls var ->
                ConstraintVariable cls (substitute sub var)
            ConstraintType cls type' args ->
                ConstraintType cls type' (fmap (substitute sub) args)

-- | Removes bound parameters from the provided list
removeBoundParams :: HS.HashSet Ident -> Params a -> Params a
removeBoundParams freeVars = filter (\(name, _) -> name `HS.member` freeVars)

-- | Generalises free variables
generaliseParams ::
       HS.HashSet Ident
    -> ParamsMap a
    -> HS.HashSet Ident
    -> Params a
    -> Params a
generaliseParams boundVars paramsMapping freeVars params =
    let existingVars = HS.fromList $ map fst params
        boundAndExistingVars = boundVars `HS.union` existingVars
        shouldGeneralise = freeVars `HS.difference` boundAndExistingVars
        newParams =
            map (\var -> (var, fromJust $ HM.lookup var paramsMapping)) $
            HS.toList shouldGeneralise
     in params ++ newParams
