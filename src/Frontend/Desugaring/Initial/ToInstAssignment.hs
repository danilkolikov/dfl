{- |
Module      :  Frontend.Desugaring.Initial.ToInstAssignment
Description :  Desugaring of AST nodes to InstAssignment
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing InstAssignment-s.
-}
module Frontend.Desugaring.Initial.ToInstAssignment
    ( DesugarToInstAssignment(..)
    ) where

import Data.List.NonEmpty (toList)

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToExp (desugarFunLHS, desugarToExp)
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Class for types which can be desugared to InstAssignment
class DesugarToInstAssignment a where
    desugarToInstAssignment ::
           a
        -> WithLocation D.InstAssignment

instance (DesugarToInstAssignment a) =>
         DesugarToInstAssignment (WithLocation a) where
    desugarToInstAssignment = (getValue . desugarToInstAssignment <$>)

instance DesugarToInstAssignment IDecl where
    desugarToInstAssignment (IDeclFunction lhs rhs) =
        withDummyLocation $
          case getValue lhs of
              Left fun ->
                  let (ident, pats) = desugarFunLHS fun
                      desugaredRHS = desugarToExp rhs
                   in D.InstAssignmentName ident (toList pats) desugaredRHS
              Right name ->
                  let desugaredName = desugarToIdent name
                      desugaredRHS = desugarToExp rhs
                   in D.InstAssignmentName desugaredName [] desugaredRHS
