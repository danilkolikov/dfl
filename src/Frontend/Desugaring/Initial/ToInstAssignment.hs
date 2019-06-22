{- |
Module      :  Frontend.Desugaring.Initial.ToInstAssignment
Description :  Desugaring of AST nodes to InstAssignment
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing InstAssignment-s.
-}
module Frontend.Desugaring.Initial.ToInstAssignment
    ( desugarToInstAssignment
    ) where

import Data.Functor (($>))
import Data.List.NonEmpty (toList)

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToExp (desugarFunLHS, desugarToExp)
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))

-- | Desugar instance eclaration to InstAssignment
desugarToInstAssignment :: WithLocation IDecl -> WithLocation D.InstAssignment
desugarToInstAssignment iDecl =
    iDecl $>
    case getValue iDecl of
        IDeclFunction lhs rhs ->
            case getValue lhs of
                Left fun ->
                    let (ident, pats) = desugarFunLHS fun
                        desugaredRHS = desugarToExp rhs
                     in D.InstAssignmentName ident (toList pats) desugaredRHS
                Right name ->
                    let desugaredName = desugarToIdent (lhs $> name)
                        desugaredRHS = desugarToExp rhs
                     in D.InstAssignmentName desugaredName [] desugaredRHS
