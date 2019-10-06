{- |
Module      :  Frontend.Inference.Let.Ast
Description :  Modified version of AST without let expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Modified version of AST with let expressions removed
-}
module Frontend.Inference.Let.Ast
    ( Const(..)
    , Ident(..)
    , GeneratedIdentEnvironment(..)
    , Expression(..)
    , Expressions
    , Exp(..)
    ) where

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Core.Ident
import Frontend.Desugaring.Ast (Const(..))
import Frontend.Inference.Signature
import Frontend.Syntax.Position

-- | Definition of an expression
data Expression = Expression
    { getExpressionName :: WithLocation Ident -- ^ Name of an expression
    , getExpressionBody :: WithLocation Exp -- ^ Body of an expression
    , getExpressionType :: Maybe TypeSignature -- ^ Optional type signature
    } deriving (Show, Eq)

-- | A map of expressions
type Expressions = HM.HashMap Ident Expression

-- | Expression
data Exp
    = ExpAbstraction (WithLocation Ident)
                     (WithLocation Exp) -- ^ Lambda-abstraction
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
