{- |
Module      :  Frontend.Desugaring.ToConstr
Description :  Desugaring of AST nodes to Constr
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Constr-s.
-}
module Frontend.Desugaring.ToConstr where

import Data.List.NonEmpty (toList)

import qualified Frontend.Desugaring.Ast as D
import Frontend.Desugaring.ToIdent (desugarToIdent)
import Frontend.Desugaring.ToType (desugarToType)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Class for types which can be desugared to Constr
class DesugarToConstr a where
    desugarToConstr :: a -> WithLocation D.Constr -- ^ Desugar object to Constr

instance (DesugarToConstr a) => DesugarToConstr (WithLocation a) where
    desugarToConstr = (getValue . desugarToConstr <$>)

instance DesugarToConstr Constr where
    desugarToConstr (ConstrSimple name args) =
        withDummyLocation $
        D.ConstrSimple (desugarToIdent name) (map desugarToType args)
    desugarToConstr (ConstrInfix l op r) =
        withDummyLocation $
        D.ConstrSimple (desugarToIdent op) (map desugarToType [l, r])
    desugarToConstr (ConstrRecord name decls) =
        withDummyLocation $
        D.ConstrRecord
            (desugarToIdent name)
            (concatMap desugarToFieldDecl decls)

-- Helper functions
desugarToFieldDecl :: WithLocation FieldDecl -> [WithLocation D.FieldDecl]
desugarToFieldDecl (WithLocation (FieldDecl vars type') loc) =
    let desugaredType = desugarToType type'
        desugarSingle var =
            WithLocation (D.FieldDecl (desugarToIdent var) desugaredType) loc
     in map desugarSingle $ toList vars
