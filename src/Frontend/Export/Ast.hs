{- |
Module      :  Frontend.Export.Ast
Description :  Result of processing of a module
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

AST of the result of the processing of a module
-}
module Frontend.Export.Ast
    ( module Frontend.Export.Ast
    , module Core.Ident
    , module Frontend.Desugaring.Final.Ast
    , module Frontend.Inference.Instance
    ) where

import Data.HashMap.Lazy (HashMap)

import Core.Ident
import Frontend.Desugaring.Final.Ast (Fixity(..))
import Frontend.Inference.Constraint
import Frontend.Inference.Instance
import Frontend.Inference.Signature
import Util.HashMap

-- | A result of module processing
data Module = Module
    { getModuleDataTypes :: DataTypes
    , getModuleTypeSynonyms :: Signatures TypeSignature
    , getModuleClasses :: Classes
    , getModuleInstances :: Instances
    , getModuleExpressions :: Expressions
    } deriving (Eq, Show)

instance Semigroup Module where
    Module d1 t1 c1 i1 e1 <> Module d2 t2 c2 i2 e2 =
        Module
            (deepMerge d1 d2)
            (t1 <> t2)
            (deepMerge c1 c2)
            (i1 <> i2)
            (e1 <> e2)

instance Monoid Module where
    mempty = Module mempty mempty mempty mempty mempty

-- | Definition of a data type
data DataType = DataType
    { getDataTypeSignature :: TypeConstructorSignature
    , getDataTypeConstructors :: HashMap Ident Constructor -- ^ List of constructors
    , isNewType :: Bool -- ^ Is this type a newtype?
    } deriving (Eq, Show)

instance Semigroup DataType where
    DataType s1 c1 i1 <> DataType s2 c2 i2 =
        if s1 == s2 && i1 == i2
            then DataType s1 (c1 <> c2) i1
            else error "Can't merge data types with different signatures"

-- | Constructor of a data type
data Constructor = Constructor
    { getConstructorExpression :: Expression
    , getConstructorFields :: FieldsMap
    } deriving (Eq, Show)

-- | A mapping from the name of a field to its position in a constructor
type FieldsMap = HashMap Ident Int

-- | A map of data types
type DataTypes = HashMap Ident DataType

-- | Definition of a type class
data Class = Class
    { getClassContext :: [SimpleConstraint] -- ^ Context of a type class
    , getClassSignature :: TypeConstructorSignature -- ^ Name of a type class
    , getClassDataTypeName :: Ident -- ^ Ident of the generated data type
    , getClassComponents :: [Ident] -- Components of a class - superclasses and methods
    , getClassGetters :: HashMap Ident Ident -- ^ Getters for methods of this and superclasses
    , getClassMethods :: HashMap Ident Expression -- ^ Methods
    , getClassDefaultInstanceName :: Ident -- ^ Ident of the generated default instance
    } deriving (Eq, Show)

instance Semigroup Class where
    Class c1 s1 d1 co1 g1 m1 di1 <> Class c2 s2 d2 co2 g2 m2 di2 =
        if c1 == c2 && s1 == s2 && d1 == d2 && co1 == co2 && di1 == di2
            then Class c1 s2 d1 co1 (g1 <> g2) (m1 <> m2) di2
            else error "Can't merge classes with different signatures"

-- | A map of classes
type Classes = HashMap Ident Class

-- | A map of defined instances
type Instances = HashMap Ident Instance

-- | Definition of an expression
data Expression = Expression
    { getExpressionType :: TypeSignature -- ^ Optional type signature
    , getExpressionFixity :: Maybe FixitySignature -- ^ Fixity of an expression
    } deriving (Eq, Show)

-- | A map of expressions
type Expressions = HashMap Ident Expression

-- | A fixity of an expression
data FixitySignature = FixitySignature
    { getFixitySignatureFixity :: Fixity -- ^ Fixity
    , getFixitySignaturePrecedence :: Int -- ^ Precedence
    } deriving (Eq, Show)
