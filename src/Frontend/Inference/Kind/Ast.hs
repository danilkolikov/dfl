{- |
Module      :  Frontend.Inference.Kind.Ast
Description :  AST with kind information
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Nodes of AST with added kind information
-}
module Frontend.Inference.Kind.Ast
    ( F.Ident(..)
    , F.IdentEnvironment(..)
    , WithKind(..)
    , KindSubstitutable(..)
    , TypeSynonym(..)
    , TypeSynonyms
    , Type(..)
    , Constraint(..)
    , SimpleConstraint(..)
    , DataType(..)
    , DataTypes
    , Constructor(..)
    , Class(..)
    , Classes
    , Method(..)
    , Methods
    , TypeSignature(..)
    ) where

import Data.Bifunctor (second)
import qualified Data.HashMap.Lazy as HM
import Data.List.NonEmpty (NonEmpty)

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Kind.Kind
import Frontend.Inference.Substitution
import Frontend.Syntax.Position (SourceLocation, WithLocation)

-- | Class for types, which support substitution of variables with kinds
class KindSubstitutable a where
    substituteKind :: Substitution Kind -> a -> a

instance KindSubstitutable F.Ident where
    substituteKind = const id

-- | Objects which have an associated kind
data WithKind a = WithKind
    { getValue :: a -- ^ Get object
    , getLocation :: SourceLocation -- ^ Get its source location
    , getKind :: Kind -- ^ Get kind
    } deriving (Eq, Show)

instance (KindSubstitutable a) => KindSubstitutable (WithKind a) where
    substituteKind sub (WithKind x loc kind) =
        WithKind (substituteKind sub x) loc (substitute sub kind)

instance (KindSubstitutable a) => KindSubstitutable (WithLocation a) where
    substituteKind sub withLoc = substituteKind sub <$> withLoc

-- | Synonym of a type
data TypeSynonym = TypeSynonym
    { getTypeSynonymName :: WithKind F.Ident -- ^ Name
    , getTypeSynonymParams :: [WithKind F.Ident] -- ^ Parameters
    , getTypeSynonymType :: WithKind Type -- ^ Type
    } deriving (Eq, Show)

instance KindSubstitutable TypeSynonym where
    substituteKind sub TypeSynonym { getTypeSynonymName = name
                                   , getTypeSynonymParams = params
                                   , getTypeSynonymType = type'
                                   } =
        TypeSynonym
            { getTypeSynonymName = substituteKind sub name
            , getTypeSynonymParams = map (substituteKind sub) params
            , getTypeSynonymType = substituteKind sub type'
            }

-- | Map of type synonyms
type TypeSynonyms = HM.HashMap F.Ident TypeSynonym

-- | Type expression
data Type
    = TypeVar (WithKind F.Ident) -- ^ Type variable
    | TypeConstr (WithKind F.Ident) -- ^ Type constructor
    | TypeFunction (WithKind Type)
                   (WithKind Type) -- ^ Function type
    | TypeApplication (WithKind Type)
                      (NonEmpty (WithKind Type)) -- ^ Application of a type constructor
    deriving (Eq, Show)

instance KindSubstitutable Type where
    substituteKind sub type' =
        case type' of
            TypeVar name -> TypeVar (substituteKind sub name)
            TypeConstr name -> TypeConstr (substituteKind sub name)
            TypeFunction from to ->
                TypeFunction (substituteKind sub from) (substituteKind sub to)
            TypeApplication func args ->
                TypeApplication
                    (substituteKind sub func)
                    (fmap (substituteKind sub) args)

-- | Type constraint
data Constraint
    = ConstraintParam { getConstraintClass :: WithLocation F.Ident -- ^ Name of a type class
                      , getConstraintParam :: WithKind F.Ident -- ^ Constrained parameter
                       } -- ^ Constrained parameter
    | ConstraintType { getConstraintClass :: WithLocation F.Ident -- ^ Name of a type class
                     , getConstraintType :: WithKind F.Ident -- ^ Name of a constrained type
                     , getConstraintParams :: NonEmpty (WithKind Type) -- ^ Parameters of a type
                      } -- ^ Constrained type
    deriving (Eq, Show)

instance KindSubstitutable Constraint where
    substituteKind sub constraint =
        case constraint of
            ConstraintParam class' param ->
                ConstraintParam class' (substituteKind sub param)
            ConstraintType class' type' params ->
                ConstraintType
                    class'
                    (substituteKind sub type')
                    (fmap (substituteKind sub) params)

-- | Simple constraint
data SimpleConstraint = SimpleConstraint
    { getSimpleConstraintClass :: WithLocation F.Ident -- ^ Type class
    , getSimpleConstraintParam :: WithKind F.Ident -- ^ Name of a constrained parameter
    } deriving (Eq, Show)

instance KindSubstitutable SimpleConstraint where
    substituteKind sub (SimpleConstraint class' param) =
        SimpleConstraint class' (substituteKind sub param)

-- | Definition of a data type
data DataType = DataType
    { getDataTypeContext :: [WithLocation Constraint] -- ^ Context of a data type
    , getDataTypeName :: WithKind F.Ident -- ^ Name of a data type
    , getDataTypeParams :: [WithKind F.Ident] -- ^ Parameters of a data type
    , getDataTypeDeriving :: [WithLocation F.Ident] -- ^ List of instances to derive
    , getDataTypeConstructors :: [(F.Ident, Constructor)] -- ^ List of constructors
    , isNewType :: Bool -- ^ Is this type a newtype?
    } deriving (Eq, Show)

instance KindSubstitutable DataType where
    substituteKind sub DataType { getDataTypeContext = context
                                , getDataTypeName = name
                                , getDataTypeParams = params
                                , getDataTypeDeriving = deriving'
                                , getDataTypeConstructors = constructors
                                , isNewType = newType
                                } =
        DataType
            { getDataTypeContext = fmap (substituteKind sub) context
            , getDataTypeName = substituteKind sub name
            , getDataTypeParams = fmap (substituteKind sub) params
            , getDataTypeDeriving = deriving'
            , getDataTypeConstructors =
                  map (second $ substituteKind sub) constructors
            , isNewType = newType
            }

-- | Map of defined data types
type DataTypes = HM.HashMap F.Ident DataType

-- | Constructor of a data type
data Constructor = Constructor
    { getConstructorName :: WithLocation F.Ident -- ^ Name of a constructor
    , getConstructorArgs :: [WithKind Type] -- ^ Arguments of a constructor
    , getConstructorFields :: HM.HashMap F.Ident Int -- ^ Map of fields of a constructor.
    } deriving (Eq, Show)

instance KindSubstitutable Constructor where
    substituteKind sub Constructor { getConstructorName = name
                                   , getConstructorArgs = args
                                   , getConstructorFields = fields
                                   } =
        Constructor
            { getConstructorName = name
            , getConstructorArgs = fmap (substituteKind sub) args
            , getConstructorFields = fields
            }

-- | Definition of a type class
data Class = Class
    { getClassContext :: [WithLocation SimpleConstraint] -- ^ Context of a type class
    , getClassName :: WithLocation F.Ident -- ^ Name of a type class
    , getClassParam :: WithKind F.Ident -- ^ Parameter of a type class
    , getClassMethods :: Methods -- ^ Methods of a type class
    } deriving (Eq, Show)

instance KindSubstitutable Class where
    substituteKind sub Class { getClassContext = context
                             , getClassName = name
                             , getClassParam = param
                             , getClassMethods = methods
                             } =
        Class
            { getClassContext = fmap (substituteKind sub) context
            , getClassName = name
            , getClassParam = substituteKind sub param
            , getClassMethods = HM.map (substituteKind sub) methods
            }

-- | Map of defined classes
type Classes = HM.HashMap F.Ident Class

-- | Definition of a class method
data Method = Method
    { getMethodName :: WithLocation F.Ident -- ^ Name of a method
    , getMethodType :: TypeSignature -- ^ Type signature
    , getMethodDefault :: Maybe (WithLocation F.Exp) -- ^ Optional default implementation
    } deriving (Eq, Show)

instance KindSubstitutable Method where
    substituteKind sub Method { getMethodName = name
                              , getMethodType = type'
                              , getMethodDefault = exp'
                              } =
        Method
            { getMethodName = name
            , getMethodType = substituteKind sub type'
            , getMethodDefault = exp'
            }

-- | Map of methods
type Methods = HM.HashMap F.Ident Method

-- | Type signature
data TypeSignature = TypeSignature
    { getTypeSignatureContext :: [WithLocation Constraint] -- ^ Context of a type
    , getTypeSignatureParams :: [WithKind F.Ident] -- ^ Free type variables, used in the type
    , getTypeSignatureType :: WithKind Type -- ^ Type
    } deriving (Eq, Show)

instance KindSubstitutable TypeSignature where
    substituteKind sub TypeSignature { getTypeSignatureContext = context
                                     , getTypeSignatureParams = params
                                     , getTypeSignatureType = type'
                                     } =
        TypeSignature
            { getTypeSignatureContext = fmap (substituteKind sub) context
            , getTypeSignatureParams = fmap (substituteKind sub) params
            , getTypeSignatureType = substituteKind sub type'
            }
