{- |
Module      :  Frontend.Desugaring.ToTopDecl
Description :  Desugaring of AST nodes to TopDecl
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing TopDecl-s.
-}
module Frontend.Desugaring.ToTopDecl
    ( DesugarToTopDecl(..)
    ) where

import qualified Frontend.Desugaring.Ast as D
import Frontend.Desugaring.IdentGenerator
import Frontend.Desugaring.ToClassAssignment (desugarToClassAssignment)
import Frontend.Desugaring.ToConstr (desugarToConstr)
import Frontend.Desugaring.ToConstraint (desugarToConstraint)
import Frontend.Desugaring.ToExp (desugarToAssignment)
import Frontend.Desugaring.ToIdent (desugarToIdent)
import Frontend.Desugaring.ToInst (desugarToInst)
import Frontend.Desugaring.ToInstAssignment (desugarToInstAssignment)
import Frontend.Desugaring.ToNewConstr (desugarToNewConstr)
import Frontend.Desugaring.ToSimpleClass (desugarToSimpleClass)
import Frontend.Desugaring.ToSimpleType (desugarToSimpleType)
import Frontend.Desugaring.ToType (desugarToType)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Class for types which can be desugared to TopDecl-s
class DesugarToTopDecl a where
    desugarToTopDecl :: a -> IdentGenerator [WithLocation D.TopDecl] -- ^ Desugar object to TopDecl-s

instance (DesugarToTopDecl a) => DesugarToTopDecl (WithLocation a) where
    desugarToTopDecl x = do
        res <- desugarToTopDecl (getValue x)
        return [getValue el <$ x | el <- res]

instance DesugarToTopDecl TopDecl where
    desugarToTopDecl (TopDeclType name type') =
        return . return . withDummyLocation $
        D.TopDeclType (desugarToSimpleType name) (desugarToType type')
    desugarToTopDecl (TopDeclData context name constrs deriving') =
        return . return . withDummyLocation $
        D.TopDeclData
            (map desugarToConstraint context)
            (desugarToSimpleType name)
            (map desugarToConstr constrs)
            (map desugarToIdent deriving')
    desugarToTopDecl (TopDeclNewType context name constr deriving') =
        return . return . withDummyLocation $
        D.TopDeclNewType
            (map desugarToConstraint context)
            (desugarToSimpleType name)
            (desugarToNewConstr constr)
            (map desugarToIdent deriving')
    desugarToTopDecl (TopDeclClass context name var methods) =
        return .
        withDummyLocation .
        D.TopDeclClass
            (map desugarToSimpleClass context)
            (desugarToIdent name)
            (desugarToIdent var) .
        concat <$>
        mapM desugarToClassAssignment methods
    desugarToTopDecl (TopDeclInstance context name inst decls) =
        return .
        withDummyLocation .
        D.TopDeclInstance
            (map desugarToSimpleClass context)
            (desugarToIdent name)
            (desugarToInst inst) <$>
        mapM desugarToInstAssignment decls
    desugarToTopDecl (TopDeclDecl decl) =
        fmap (withDummyLocation . D.TopDeclAssignment) <$>
        desugarToAssignment decl
