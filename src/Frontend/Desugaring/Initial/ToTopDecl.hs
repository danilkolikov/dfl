{- |
Module      :  Frontend.Desugaring.Initial.ToTopDecl
Description :  Desugaring of AST nodes to TopDecl
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing TopDecl-s.
-}
module Frontend.Desugaring.Initial.ToTopDecl
    ( DesugarToTopDecl(..)
    ) where

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
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Class for types which can be desugared to TopDecl-s
class DesugarToTopDecl a where
    desugarToTopDecl :: a -> [WithLocation D.TopDecl] -- ^ Desugar object to TopDecl-s

instance (DesugarToTopDecl a) => DesugarToTopDecl (WithLocation a) where
    desugarToTopDecl = sequence . ((getValue <$>) . desugarToTopDecl <$>)

instance DesugarToTopDecl TopDecl where
    desugarToTopDecl (TopDeclType name type') =
        return . withDummyLocation $
        D.TopDeclType (desugarToSimpleType name) (desugarToType type')
    desugarToTopDecl (TopDeclData context name constrs deriving') =
        return . withDummyLocation $
        D.TopDeclData
            (map desugarToConstraint context)
            (desugarToSimpleType name)
            (map desugarToConstr constrs)
            (map desugarToIdent deriving')
    desugarToTopDecl (TopDeclNewType context name constr deriving') =
        return . withDummyLocation $
        D.TopDeclNewType
            (map desugarToConstraint context)
            (desugarToSimpleType name)
            (desugarToNewConstr constr)
            (map desugarToIdent deriving')
    desugarToTopDecl (TopDeclClass context name var methods) =
        return . withDummyLocation $
        D.TopDeclClass
            (map desugarToSimpleClass context)
            (desugarToIdent name)
            (desugarToIdent var)
            (concatMap desugarToClassAssignment methods)
    desugarToTopDecl (TopDeclInstance context name inst decls) =
        return . withDummyLocation $
        D.TopDeclInstance
            (map desugarToSimpleClass context)
            (desugarToIdent name)
            (desugarToInst inst)
            (map desugarToInstAssignment decls)
    desugarToTopDecl (TopDeclDecl decl) =
        map (withDummyLocation . D.TopDeclAssignment) $ desugarToAssignment decl
