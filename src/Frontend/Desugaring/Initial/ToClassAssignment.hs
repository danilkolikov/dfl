{- |
Module      :  Frontend.Desugaring.Initial.ToClassAssignment
Description :  Desugaring of AST nodes to ClassAssignment
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing ClassAssignment-s.
-}
module Frontend.Desugaring.Initial.ToClassAssignment
    ( desugarToClassAssignment
    ) where

import Data.Functor (($>))
import Data.List.NonEmpty (toList)

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToExp
    ( desugarFunLHS
    , desugarGenDecl
    , desugarToExp
    )
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))

-- | Desugar class declaration to ClassAssignment
desugarToClassAssignment ::
       WithLocation CDecl -> [WithLocation D.ClassAssignment]
desugarToClassAssignment cDecl =
    map (cDecl $>) $
    case getValue cDecl of
        CDeclGenDecl genDecl -> desugarGenDecl D.ClassAssignmentType genDecl
        CDeclFunction lhs rhs ->
            [ case getValue lhs of
                  Left fun ->
                      let (ident, pats) = desugarFunLHS fun
                          desugaredRHS = desugarToExp rhs
                       in D.ClassAssignmentName ident (toList pats) desugaredRHS
                  Right name ->
                      let desugaredName = desugarToIdent (lhs $> name)
                          desugaredRHS = desugarToExp rhs
                       in D.ClassAssignmentName desugaredName [] desugaredRHS
            ]
