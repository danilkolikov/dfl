{- |
Module      :  Frontend.Desugaring.Initial.ToConstr
Description :  Desugaring of AST nodes to Constr
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Constr-s.
-}
module Frontend.Desugaring.Initial.ToConstr
    ( desugarToConstr
    , desugarToFieldDecl
    ) where

import Data.Functor (($>))
import Data.List.NonEmpty (toList)

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Desugaring.Initial.ToType (desugarToType)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))

-- | Desugar object to Constr
desugarToConstr :: WithLocation Constr -> WithLocation D.Constr
desugarToConstr constr =
    constr $>
    case getValue constr of
        ConstrSimple name args ->
            D.ConstrSimple (desugarToIdent name) (map desugarToType args)
        ConstrInfix l op r ->
            D.ConstrSimple (desugarToIdent op) (map desugarToType [l, r])
        ConstrRecord name decls ->
            D.ConstrRecord
                (desugarToIdent name)
                (concatMap desugarToFieldDecl decls)

-- Helper functions
-- | Desugar field declaration
desugarToFieldDecl :: WithLocation FieldDecl -> [WithLocation D.FieldDecl]
desugarToFieldDecl (WithLocation (FieldDecl vars type') loc) =
    let desugaredType = desugarToType type'
        desugarSingle var =
            WithLocation (D.FieldDecl (desugarToIdent var) desugaredType) loc
     in map desugarSingle $ toList vars
