{- |
Module      :  Frontend.Desugaring.Initial.ToClassAssignment
Description :  Desugaring of AST nodes to ClassAssignment
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing ClassAssignment-s.
-}
module Frontend.Desugaring.Initial.ToClassAssignment where

import Data.List.NonEmpty (toList)

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToExp
    ( desugarFunLHS
    , desugarGenDecl
    , desugarToExp
    )
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Class for types which can be desugared to ClassAssignment
class DesugarToClassAssignment a where
    desugarToClassAssignment :: a -> [WithLocation D.ClassAssignment]

instance (DesugarToClassAssignment a) =>
         DesugarToClassAssignment (WithLocation a) where
    desugarToClassAssignment =
        sequence . ((getValue <$>) . desugarToClassAssignment <$>)

instance DesugarToClassAssignment CDecl where
    desugarToClassAssignment (CDeclGenDecl genDecl) =
        desugarToClassAssignment genDecl
    desugarToClassAssignment (CDeclFunction lhs rhs) =
        [ withDummyLocation $
          case getValue lhs of
              Left fun ->
                  let (ident, pats) = desugarFunLHS fun
                      desugaredRHS = desugarToExp rhs
                   in D.ClassAssignmentName ident (toList pats) desugaredRHS
              Right name ->
                  let desugaredName = desugarToIdent name
                      desugaredRHS = desugarToExp rhs
                   in D.ClassAssignmentName desugaredName [] desugaredRHS
        ]

instance DesugarToClassAssignment GenDecl where
    desugarToClassAssignment = desugarGenDecl D.ClassAssignmentType
