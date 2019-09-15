{- |
Module      :  Frontend.Inference.Kind.Ast
Description :  Modified version of AST with kind information
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Modified version of AST, produced by kind inference with included kind information
-}
module Frontend.Inference.Kind.Ast where

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Final.Ast (Const, Ident, SimpleConstraint)
import Frontend.Inference.Signature
import Frontend.Syntax.Position

-- | A slice of a module with kind information
data AstWithKinds = AstWithKinds
    { getAstWithKindsClasses :: HM.HashMap Ident Class
    , getAstWithKindsInstances :: [Instance]
    , getAstWithKindsExpressions :: HM.HashMap Ident Expression
    }

-- | Definition of a type class
data Class = Class
    { getClassContext :: [WithLocation SimpleConstraint] -- ^ Context of a type class
    , getClassName :: WithLocation Ident -- ^ Name of a type class
    , getClassParam :: WithLocation Ident -- ^ Parameter of a type class
    , getClassMethods :: HM.HashMap Ident Method -- ^ Methods of a type class
    } deriving (Show, Eq)

-- | Definition of an instance of a type class
data Instance = Instance
    { getInstanceContext :: [WithLocation SimpleConstraint] -- ^ Context of an instance
    , getInstanceClass :: WithLocation Ident -- ^ Name of a type class
    , getInstanceType :: WithLocation Ident -- ^ Name of a type
    , getInstanceTypeArgs :: [WithLocation Ident] -- ^ Arguments of a type
    , getInstanceMethods :: HM.HashMap Ident Expression -- ^ Methods of an instance
    } deriving (Show, Eq)

-- | Definition of an expression
data Expression = Expression
    { getExpressionName :: WithLocation Ident -- ^ Name of an expression
    , getExpressionBody :: WithLocation Exp -- ^ Body of an expression
    , getExpressionType :: Maybe TypeSignature -- ^ Optional type signature
    } deriving (Show, Eq)

-- | Definition of a class method
data Method = Method
    { getMethodName :: WithLocation Ident -- ^ Name of a method
    , getMethodType :: TypeSignature -- ^ Type signature
    , getMethodDefault :: Maybe (WithLocation Exp) -- ^ Optional default implementation
    } deriving (Show, Eq)

-- | Expression
data Exp
    = ExpAbstraction (WithLocation Ident)
                     (WithLocation Exp) -- ^ Lambda-abstraction
    | ExpLet (HM.HashMap Ident Expression)
             (WithLocation Exp) -- ^ Let-abstraction
    | ExpCase (WithLocation Ident)
              (WithLocation Ident)
              [WithLocation Ident]
              (WithLocation Exp)
              (WithLocation Ident) -- ^ Case expression
    | ExpApplication (WithLocation Exp)
                     (NE.NonEmpty (WithLocation Exp)) -- ^ Application of expressions
    | ExpVar (WithLocation Ident) -- ^ Variable
    | ExpConstr (WithLocation Ident) -- ^ Constructor
    | ExpConst (WithLocation Const) -- ^ Constant
    deriving (Show, Eq)
