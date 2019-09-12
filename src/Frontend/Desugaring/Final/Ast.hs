{- |
Module      :  Frontend.Desugaring.Final.Ast
Description :  Final desugared version of AST of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugared version of AST of DFL.
-}
module Frontend.Desugaring.Final.Ast
    ( Ident(..)
    , IdentEnvironment(..)
    , Const(..)
    , Module(..)
    , ImpExpList(..)
    , Export(..)
    , ImpDecl(..)
    , Import(..)
    , TypeSynonym(..)
    , TypeSynonyms
    , DataType(..)
    , DataTypes
    , Constructor(..)
    , Class(..)
    , Classes
    , Instance(..)
    , Instances
    , Constraint(..)
    , SimpleConstraint(..)
    , TypeSignature(..)
    , Type(..)
    , Expression(..)
    , Expressions
    , Method(..)
    , Methods
    , Exp(..)
    ) where

import Data.HashMap.Lazy (HashMap)
import Data.List.NonEmpty (NonEmpty)

import Frontend.Desugaring.Initial.Ast
    ( Const(..)
    , Export(..)
    , Ident(..)
    , IdentEnvironment(..)
    , ImpDecl(..)
    , ImpExpList(..)
    , Import(..)
    , Type(..)
    )
import Frontend.Syntax.Position (WithLocation)

-- | Definition of a module
data Module = Module
    { getModuleName :: WithLocation Ident -- ^ Name of a module
    , getModuleExports :: ImpExpList (WithLocation Export) -- ^ Exports
    , getModuleImports :: [WithLocation ImpDecl] -- ^ Imports
    , getModuleTypeSynonyms :: TypeSynonyms -- ^ Type synonyms
    , getModuleDataTypes :: DataTypes -- ^ Data types
    , getModuleClasses :: Classes -- ^ Classes
    , getModuleInstances :: Instances -- ^ Instances
    , getModuleExpressions :: Expressions -- ^ Expressions
    } deriving (Show, Eq)

-- | Definition of a type synonym
data TypeSynonym = TypeSynonym
    { getTypeSynonymName :: WithLocation Ident -- ^ Name of a type synonym
    , getTypeSynonymParams :: [WithLocation Ident] -- ^ Parameters of a type synonym
    , getTypeSynonymType :: WithLocation Type -- ^ Type
    } deriving (Show, Eq)

-- | Map of type synonyms
type TypeSynonyms = HashMap Ident TypeSynonym

-- | Definition of a data type
data DataType = DataType
    { getDataTypeContext :: [WithLocation Constraint] -- ^ Context of a data type
    , getDataTypeName :: WithLocation Ident -- ^ Name of a data type
    , getDataTypeParams :: [WithLocation Ident] -- ^ Parameters of a data type
    , getDataTypeDeriving :: [WithLocation Ident] -- ^ List of instances to derive
    , getDataTypeConstructors :: [(Ident, Constructor)] -- ^ List of constructors
    , isNewType :: Bool -- ^ Is this type a newtype?
    } deriving (Show, Eq)

-- | Map of data types
type DataTypes = HashMap Ident DataType

-- | Constructor of a data type
data Constructor = Constructor
    { getConstructorName :: WithLocation Ident -- ^ Name of a constructor
    , getConstructorArgs :: [WithLocation Type] -- ^ Arguments of a constructor
    , getConstructorFields :: HashMap Ident Int -- ^ Map of fields of a constructor.
    } deriving (Show, Eq)

-- | Type constraint
data Constraint
    = ConstraintParam { getConstraintClass :: WithLocation Ident -- ^ Name of a type class
                      , getConstraintParam :: WithLocation Ident -- ^ Constrained parameter
                       } -- ^ Constrained parameter
    | ConstraintAppliedParam { getConstraintClass :: WithLocation Ident -- ^ Name of a type class
                             , getConstraintParam :: WithLocation Ident -- ^ Name of a constrained variable
                             , getConstraintArgs :: NonEmpty (WithLocation Type) -- ^ Parameters of a variable
                              } -- ^ Constrained type
    deriving (Show, Eq)

-- | Simple constraint
data SimpleConstraint = SimpleConstraint
    { getSimpleConstraintClass :: WithLocation Ident -- ^ Name of a type class
    , getSimpleConstraintType :: WithLocation Ident -- ^ Name of a constrained type
    } deriving (Show, Eq)

-- | Definition of a type class
data Class = Class
    { getClassContext :: [WithLocation SimpleConstraint] -- ^ Context of a type class
    , getClassName :: WithLocation Ident -- ^ Name of a type class
    , getClassParam :: WithLocation Ident -- ^ Parameter of a type class
    , getClassMethods :: Methods -- ^ Methods of a type class
    } deriving (Show, Eq)

-- | Map of classes
type Classes = HashMap Ident Class

-- | Definition of an instance of a type class
data Instance = Instance
    { getInstanceContext :: [WithLocation SimpleConstraint] -- ^ Context of an instance
    , getInstanceClass :: WithLocation Ident -- ^ Name of a type class
    , getInstanceType :: WithLocation Ident -- ^ Name of a type
    , getInstanceTypeArgs :: [WithLocation Ident] -- ^ Arguments of a type
    , getInstanceMethods :: Expressions -- ^ Methods of an instance
    } deriving (Show, Eq)

-- | Map of instances
type Instances = [Instance]

-- | Type signature
data TypeSignature = TypeSignature
    { getTypeSignatureContext :: [WithLocation Constraint] -- ^ Context of a type
    , getTypeSignatureType :: WithLocation Type -- ^ Type
    } deriving (Show, Eq)

-- | Definition of an expression
data Expression = Expression
    { getExpressionName :: WithLocation Ident -- ^ Name of an expression
    , getExpressionBody :: WithLocation Exp -- ^ Body of an expression
    , getExpressionType :: Maybe TypeSignature -- ^ Optional type signature
    } deriving (Show, Eq)

-- | Map of expressions
type Expressions = HashMap Ident Expression

-- | Definition of a class method
data Method = Method
    { getMethodName :: WithLocation Ident -- ^ Name of a method
    , getMethodType :: TypeSignature -- ^ Type signature
    , getMethodDefault :: Maybe (WithLocation Exp) -- ^ Optional default implementation
    } deriving (Show, Eq)

-- | Map of methods
type Methods = HashMap Ident Method

-- | Expression
data Exp
    = ExpAbstraction (WithLocation Ident)
                     (WithLocation Exp) -- ^ Lambda-abstraction
    | ExpLet Expressions
             (WithLocation Exp) -- ^ Let-abstraction
    | ExpCase (WithLocation Ident)
              (WithLocation Ident)
              [WithLocation Ident]
              (WithLocation Exp)
              (WithLocation Ident) -- ^ Case expression
    | ExpApplication (WithLocation Exp)
                     (NonEmpty (WithLocation Exp)) -- ^ Application of expressions
    | ExpVar (WithLocation Ident) -- ^ Variable
    | ExpConstr (WithLocation Ident) -- ^ Constructor
    | ExpConst (WithLocation Const) -- ^ Constant
    deriving (Show, Eq)
