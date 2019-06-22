{- |
Module      :  Frontend.Desugaring.Initial.ToTopDecl
Description :  Desugaring of AST nodes to TopDecl
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing TopDecl-s.
-}
module Frontend.Desugaring.Initial.ToTopDecl
    ( desugarToTopDecl
    ) where

import Data.Functor (($>))

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToClassAssignment (desugarToClassAssignment)
import Frontend.Desugaring.Initial.ToConstr (desugarToConstr)
import Frontend.Desugaring.Initial.ToConstraint (desugarToConstraint)
import Frontend.Desugaring.Initial.ToExp (desugarToAssignment)
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Desugaring.Initial.ToInst (desugarToInst)
import Frontend.Desugaring.Initial.ToInstAssignment (desugarToInstAssignment)
import Frontend.Desugaring.Initial.ToNewConstr (desugarToNewConstr)
import Frontend.Desugaring.Initial.ToSimpleClass (desugarToSimpleClass)
import Frontend.Desugaring.Initial.ToSimpleType (desugarToSimpleType)
import Frontend.Desugaring.Initial.ToType (desugarToType)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))

-- ^ Desugar object to TopDecl-s
desugarToTopDecl :: WithLocation TopDecl -> [WithLocation D.TopDecl]
desugarToTopDecl topDecl =
    map (topDecl $>) $
    case getValue topDecl of
        TopDeclType name type' ->
            return $
            D.TopDeclType (desugarToSimpleType name) (desugarToType type')
        TopDeclData context name constrs deriving' ->
            return $
            D.TopDeclData
                (map desugarToConstraint context)
                (desugarToSimpleType name)
                (map desugarToConstr constrs)
                (map desugarToIdent deriving')
        TopDeclNewType context name constr deriving' ->
            return $
            D.TopDeclNewType
                (map desugarToConstraint context)
                (desugarToSimpleType name)
                (desugarToNewConstr constr)
                (map desugarToIdent deriving')
        TopDeclClass context name var methods ->
            return $
            D.TopDeclClass
                (map desugarToSimpleClass context)
                (desugarToIdent name)
                (desugarToIdent var)
                (concatMap desugarToClassAssignment methods)
        TopDeclInstance context name inst decls ->
            return $
            D.TopDeclInstance
                (map desugarToSimpleClass context)
                (desugarToIdent name)
                (desugarToInst inst)
                (map desugarToInstAssignment decls)
        TopDeclDecl decl -> map D.TopDeclAssignment $ desugarToAssignment decl
