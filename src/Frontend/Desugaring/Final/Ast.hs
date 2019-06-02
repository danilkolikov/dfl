{- |
Module      :  Frontend.Desugaring.Final.Ast
Description :  Final desugared version of AST of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugared version of AST of DFL.
-}
module Frontend.Desugaring.Final.Ast
    ( Ident(..)
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
    , NewType(..)
    , NewTypes
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
    , Exp(..)
    , Alt(..)
    , Pattern(..)
    ) where

import Data.HashMap.Lazy (HashMap)
import Data.List.NonEmpty (NonEmpty)

import Frontend.Desugaring.Initial.Ast
    ( Const(..)
    , Export(..)
    , Ident(..)
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
    , getModuleNewTypes :: NewTypes -- ^ New types
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
    } deriving (Show, Eq)

-- | Map of data types
type DataTypes = HashMap Ident DataType

-- | Constructor of a data type
data Constructor = Constructor
    { getConstructorName :: WithLocation Ident -- ^ Name of a constructor
    , getConstructorArgs :: [WithLocation Type] -- ^ Arguments of a constructor
    , getConstructorFields :: HashMap Ident Int -- ^ Map of fields of a constructor.
    } deriving (Show, Eq)

-- | Definition of a new type
data NewType = NewType
    { getNewTypeContext :: [WithLocation Constraint] -- ^ Context of a new type
    , getNewTypeName :: WithLocation Ident -- ^ Name of a new type
    , getNewTypeParams :: [WithLocation Ident] -- ^ Parameters of a new type
    , getNewTypeDeriving :: [WithLocation Ident] -- ^ List of instances to derive
    , getNewTypeConstructor :: Constructor -- ^ Constructor
    } deriving (Show, Eq)

-- | Map of newtypes
type NewTypes = HashMap Ident NewType

-- | Type constraint
data Constraint = Constraint
    { getConstraintClass :: WithLocation Ident -- ^ Name of a type class
    , getConstraintType :: WithLocation Ident -- ^ Name of a constrained type
    , getConstraintTypeArgs :: [WithLocation Type] -- ^ Optional arguments of a type
    } deriving (Show, Eq)

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
    , getClassMethods :: Expressions -- ^ Methods of a type class
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
type Instances = HashMap Ident Instance

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

-- | Expression
data Exp
    = ExpAbstraction (NonEmpty (WithLocation Ident))
                     (WithLocation Exp) -- ^ Lambda-abstraction
    | ExpLet Expressions
             (WithLocation Exp) -- ^ Let-abstraction
    | ExpCase (WithLocation Ident)
              (NonEmpty (WithLocation Alt)) -- ^ Case expression
    | ExpApplication (WithLocation Exp)
                     (NonEmpty (WithLocation Exp)) -- ^ Application of expressions
    | ExpVar (WithLocation Ident) -- ^ Variable
    | ExpConst (WithLocation Const) -- ^ Constant
    deriving (Show, Eq)

-- | Alternative in `case` expressions
data Alt =
    Alt (WithLocation Pattern) -- ^ Pattern
        (WithLocation Exp) -- ^ Expression
    deriving (Show, Eq)

-- | Pattern
data Pattern
    = PatternConstr (WithLocation Ident)
                    [WithLocation Ident] -- ^ Application of a constructor
    | PatternVar (WithLocation Ident) -- ^ Variable with possible pattern
    | PatternConst (WithLocation Const) -- ^ Constant
    | PatternWildcard -- ^ Wildcard
    deriving (Show, Eq)
