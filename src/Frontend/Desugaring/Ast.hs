{- |
Module      :  Frontend.Desugaring.Ast
Description :  Desugared version of AST of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugared version of AST of DFL. Nodes are parametrised by the type of expressions
-}
module Frontend.Desugaring.Ast
    ( module Frontend.Desugaring.Ast
    , module Frontend.Desugaring.Initial.Ast
    , module Core.Ident
    ) where

import Data.HashMap.Lazy (HashMap)
import Data.List.NonEmpty (NonEmpty)

import Core.Ident
import Frontend.Desugaring.Initial.Ast (Const(..), Fixity(..), ImpExpList(..))
import Frontend.Syntax.Position (WithLocation)
import Util.HashMap

-- | Definition of a module
data Module e = Module
    { getModuleName :: WithLocation Ident -- ^ Name of a module
    , getModuleExports :: Exports -- ^ Exports
    , getModuleTypeSynonyms :: TypeSynonyms -- ^ Type synonyms
    , getModuleDataTypes :: DataTypes -- ^ Data types
    , getModuleClasses :: Classes e -- ^ Classes
    , getModuleInstances :: Instances e -- ^ Instances
    , getModuleExpressions :: Expressions e -- ^ Expressions
    } deriving (Eq, Show)

-- | A list of exports
type Exports = ImpExpList (WithLocation Export)

-- | Exports of a module
data Export
    = ExportFunction (WithLocation Ident) -- ^ Export single function
    | ExportDataOrClass (WithLocation Ident)
                        (ImpExpList (WithLocation Ident)) -- ^ Export data or class
    | ExportModule (WithLocation Ident) -- ^ Export module
    deriving (Show, Eq)

-- | Definition of a type synonym
data TypeSynonym = TypeSynonym
    { getTypeSynonymName :: WithLocation Ident -- ^ Name of a type synonym
    , getTypeSynonymParams :: [WithLocation Ident] -- ^ Parameters of a type synonym
    , getTypeSynonymType :: WithLocation Type -- ^ Type
    } deriving (Eq, Show)

-- | A map of type synonyms
type TypeSynonyms = HashMap Ident TypeSynonym

-- | Definition of a data type
data DataType = DataType
    { getDataTypeContext :: [WithLocation Constraint] -- ^ Context of a data type
    , getDataTypeName :: WithLocation Ident -- ^ Name of a data type
    , getDataTypeParams :: [WithLocation Ident] -- ^ Parameters of a data type
    , getDataTypeDeriving :: [WithLocation Ident] -- ^ List of instances to derive
    , getDataTypeConstructors :: [(Ident, Constructor)] -- ^ List of constructors
    , isNewType :: Bool -- ^ Is this type a newtype?
    } deriving (Eq, Show)

-- | A map of data types
type DataTypes = HashMap Ident DataType

-- | Constructor of a data type
data Constructor = Constructor
    { getConstructorName :: WithLocation Ident -- ^ Name of a constructor
    , getConstructorArgs :: [WithLocation Type] -- ^ Arguments of a constructor
    , getConstructorFixity :: Maybe FixitySignature -- ^ Optional fixity of a constructor
    , getConstructorFields :: HashMap Ident Int -- ^ Map of fields of a constructor.
    } deriving (Eq, Show)

-- | Type constraint
data Constraint
    = ConstraintParam { getConstraintClass :: WithLocation Ident -- ^ Name of a type class
                      , getConstraintParam :: WithLocation Ident -- ^ Constrained parameter
                       } -- ^ Constrained parameter
    | ConstraintAppliedParam { getConstraintClass :: WithLocation Ident -- ^ Name of a type class
                             , getConstraintParam :: WithLocation Ident -- ^ Name of a constrained variable
                             , getConstraintArgs :: NonEmpty (WithLocation Type) -- ^ Parameters of a variable
                              } -- ^ Constrained type
    deriving (Eq, Show)

-- | Simple constraint
data SimpleConstraint = SimpleConstraint
    { getSimpleConstraintClass :: WithLocation Ident -- ^ Name of a type class
    , getSimpleConstraintType :: WithLocation Ident -- ^ Name of a constrained type
    } deriving (Eq, Show)

-- | Definition of a type class
data Class e = Class
    { getClassContext :: [WithLocation SimpleConstraint] -- ^ Context of a type class
    , getClassName :: WithLocation Ident -- ^ Name of a type class
    , getClassParam :: WithLocation Ident -- ^ Parameter of a type class
    , getClassMethods :: Methods e -- ^ Methods of a type class
    } deriving (Eq, Show)

-- | A map of classes
type Classes e = HashMap Ident (Class e)

-- | Definition of an instance of a type class
data Instance e = Instance
    { getInstanceContext :: [WithLocation SimpleConstraint] -- ^ Context of an instance
    , getInstanceClass :: WithLocation Ident -- ^ Name of a type class
    , getInstanceType :: WithLocation Ident -- ^ Name of a type
    , getInstanceTypeArgs :: [WithLocation Ident] -- ^ Arguments of a type
    , getInstanceMethods :: Exps e -- ^ Methods of an instance
    } deriving (Eq, Show)

-- | A map of raw expressions
type Exps e = HashMap Ident (WithLocation e)

-- | A list of instances
type Instances e = [Instance e]

-- | Type signature
data TypeSignature = TypeSignature
    { getTypeSignatureContext :: [WithLocation Constraint] -- ^ Context of a type
    , getTypeSignatureType :: WithLocation Type -- ^ Type
    } deriving (Eq, Show)

-- | Fixity signature of an expression
data FixitySignature = FixitySignature
    { getFixitySignatureName :: WithLocation Ident -- ^ Name of an operator
    , getFixitySignatureFixity :: Fixity -- ^ Fixity
    , getFixitySignaturePrecedence :: Int -- ^ Precedence
    } deriving (Eq, Show)

-- | Definition of an expression
data Expression e = Expression
    { getExpressionName :: WithLocation Ident -- ^ Name of an expression
    , getExpressionBody :: WithLocation e -- ^ Body of an expression
    , getExpressionType :: Maybe TypeSignature -- ^ Optional type signature
    , getExpressionFixity :: Maybe FixitySignature -- ^ Fixity of an expression
    } deriving (Eq, Show)

-- | A map of expressions
type Expressions e = HashMap Ident (Expression e)

-- | Definition of a class method
data Method e = Method
    { getMethodName :: WithLocation Ident -- ^ Name of a method
    , getMethodType :: TypeSignature -- ^ Type signature
    , getMethodBody :: Maybe (WithLocation e) -- ^ Optional default implementation
    , getMethodFixity :: Maybe FixitySignature -- ^ Fixity of an expression
    } deriving (Eq, Show)

-- | A map of methods
type Methods e = HashMap Ident (Method e)

-- | Type
data Type
    = TypeApplication (WithLocation Type)
                      (NonEmpty (WithLocation Type)) -- ^ Application of a type constructor
    | TypeFunction (WithLocation Type)
                   (WithLocation Type) -- ^ Function type
    | TypeVar (WithLocation Ident) -- ^ Type variable
    | TypeConstr (WithLocation Ident) -- ^ Type constructor
    deriving (Show, Eq)

-- | Class for types which contain expressions
class MapExpression a where
    mapExpressionM ::
           (Monad m)
        => (WithLocation e1 -> m (WithLocation e2))
        -> a e1
        -> m (a e2)

instance MapExpression Expression where
    mapExpressionM f Expression { getExpressionName = name
                                , getExpressionType = type'
                                , getExpressionFixity = fixity
                                , getExpressionBody = body
                                } = do
        res <- f body
        return
            Expression
                { getExpressionName = name
                , getExpressionType = type'
                , getExpressionFixity = fixity
                , getExpressionBody = res
                }

instance MapExpression Method where
    mapExpressionM f Method { getMethodName = name
                            , getMethodType = type'
                            , getMethodFixity = fixity
                            , getMethodBody = body
                            } = do
        res <- traverse f body
        return
            Method
                { getMethodName = name
                , getMethodType = type'
                , getMethodFixity = fixity
                , getMethodBody = res
                }

instance MapExpression Class where
    mapExpressionM f Class { getClassContext = context
                           , getClassName = name
                           , getClassParam = param
                           , getClassMethods = methods
                           } = do
        res <- mapHashMapM (mapExpressionM f) methods
        return
            Class
                { getClassContext = context
                , getClassName = name
                , getClassParam = param
                , getClassMethods = res
                }

instance MapExpression Instance where
    mapExpressionM f Instance { getInstanceContext = context
                              , getInstanceClass = className
                              , getInstanceType = typeName
                              , getInstanceTypeArgs = args
                              , getInstanceMethods = exps
                              } = do
        res <- mapHashMapM f exps
        return
            Instance
                { getInstanceContext = context
                , getInstanceClass = className
                , getInstanceType = typeName
                , getInstanceTypeArgs = args
                , getInstanceMethods = res
                }

instance MapExpression Module where
    mapExpressionM f Module { getModuleName = name
                            , getModuleExports = exports
                            , getModuleDataTypes = dataTypes
                            , getModuleTypeSynonyms = typeSynonyms
                            , getModuleClasses = classes
                            , getModuleInstances = instances
                            , getModuleExpressions = expressions
                            } = do
        classesRes <- mapHashMapM (mapExpressionM f) classes
        instancesRes <- mapM (mapExpressionM f) instances
        expressionsRes <- mapHashMapM (mapExpressionM f) expressions
        return
            Module
                { getModuleName = name
                , getModuleExports = exports
                , getModuleDataTypes = dataTypes
                , getModuleTypeSynonyms = typeSynonyms
                , getModuleClasses = classesRes
                , getModuleInstances = instancesRes
                , getModuleExpressions = expressionsRes
                }
