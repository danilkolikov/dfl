{- |
Module      :  Frontend.Desugaring.Final.Ast
Description :  Final desugared version of AST of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugared version of AST of DFL.
-}
module Frontend.Desugaring.Final.Ast
    ( module Frontend.Desugaring.Final.Ast
    , module Frontend.Desugaring.Ast
    ) where

import Data.List.NonEmpty (NonEmpty)

import Frontend.Desugaring.Ast
import Frontend.Syntax.Position (WithLocation)

-- | Expression
data Exp
    = ExpAbstraction (WithLocation Ident)
                     (WithLocation Exp) -- ^ Lambda-abstraction
    | ExpLet (Expressions Exp)
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
