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
    ) where

import Data.HashMap.Lazy (HashMap)

import Core.Ident
import Frontend.Desugaring.Final.Ast (Fixity(..))
import Frontend.Inference.Constraint
import Frontend.Inference.Instance
import Frontend.Inference.Signature

-- | A result of module processing
data Module = Module
    { getModuleDataTypes :: DataTypes
    , getModuleTypeSynonyms :: Signatures TypeSignature
    , getModuleClasses :: Classes
    , getModuleInstances :: Instances
    , getModuleExpressions :: Expressions
    } deriving (Eq, Show)

-- | Definition of a data type
data DataType = DataType
    { getDataTypeSignature :: TypeConstructorSignature
    , getDataTypeConstructors :: [(Ident, Constructor)] -- ^ List of constructors
    , isNewType :: Bool -- ^ Is this type a newtype?
    } deriving (Eq, Show)

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
    , getClassGetters :: HashMap Ident Ident -- ^ Getters for methods of this and superclasses
    , getClassMethods :: [(Ident, Expression)] -- ^ List of methods
    , getClassDefaultInstanceName :: Ident -- ^ Ident of the generated default instance
    } deriving (Eq, Show)

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
