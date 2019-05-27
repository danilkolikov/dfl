{- |
Module      :  Frontend.Desugaring.ToIdent
Description :  Desugaring of AST nodes to Ident
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Ident-s.
-}
module Frontend.Desugaring.ToIdent
    ( DesugarToIdent(..)
    ) where

import Data.List (stripPrefix)

import Frontend.Desugaring.Ast
import Frontend.Desugaring.IdentGenerator (gENERATED_ID_PREFIX)
import Frontend.Syntax.Ast
    ( DClass(..)
    , FuncLabel(..)
    , GCon(..)
    , GConSym(..)
    , GTyCon(..)
    , OpLabel(..)
    , Qualified(..)
    )
import Frontend.Syntax.EntityName
import Frontend.Syntax.NamedEntity (NamedEntity(..))
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)
import Frontend.Syntax.Token (ConId, ConSym, VarId, VarSym)

-- | Class for types which can be desugared to Ident-s
class DesugarToIdent a where
    desugarToIdent :: a -> WithLocation Ident -- ^ Desugar object to Ident

instance (NamedEntity a, NamedEntity b) => DesugarToIdent (OpLabel a b) where
    desugarToIdent = desugarNamedIdent

instance (NamedEntity a, NamedEntity b) => DesugarToIdent (FuncLabel a b) where
    desugarToIdent = desugarNamedIdent

instance (NamedEntity a, NamedEntity b) => DesugarToIdent (Either a b) where
    desugarToIdent = desugarNamedIdent

instance (DesugarToIdent a) => DesugarToIdent (WithLocation a) where
    desugarToIdent = (getValue . desugarToIdent <$>)

instance DesugarToIdent ConId where
    desugarToIdent = desugarNamedIdent

instance DesugarToIdent ConSym where
    desugarToIdent = desugarNamedIdent

instance DesugarToIdent VarId where
    desugarToIdent = desugarNamedIdent

instance DesugarToIdent VarSym where
    desugarToIdent = desugarNamedIdent

instance (NamedEntity a) => DesugarToIdent (Qualified a) where
    desugarToIdent = desugarNamedIdent

instance DesugarToIdent GConSym where
    desugarToIdent = desugarNamedIdent

instance DesugarToIdent GCon where
    desugarToIdent (GConNamed name) = desugarToIdent name
    desugarToIdent GConUnit = withDummyLocation $ IdentNamed uNIT_NAME
    desugarToIdent GConList = withDummyLocation $ IdentNamed lIST_NAME
    desugarToIdent (GConTuple n) =
        withDummyLocation $ IdentParametrised tUPLE_NAME n

instance DesugarToIdent GTyCon where
    desugarToIdent (GTyConNamed name) = desugarToIdent name
    desugarToIdent GTyConUnit = withDummyLocation $ IdentNamed uNIT_NAME
    desugarToIdent GTyConList = withDummyLocation $ IdentNamed lIST_NAME
    desugarToIdent (GTyConTuple n) =
        withDummyLocation $ IdentParametrised tUPLE_NAME n
    desugarToIdent GTyConFunction = withDummyLocation $ IdentNamed fUNCTION_NAME

instance DesugarToIdent DClass where
    desugarToIdent (DClass name) = desugarToIdent name

-- Helper functions
desugarNamedIdent :: (NamedEntity a) => a -> WithLocation Ident
desugarNamedIdent x =
    withDummyLocation $
    let entityName = getEntityName x
     in case entityName of
            [] -> undefined
            [name] ->
                case stripPrefix gENERATED_ID_PREFIX name of
                    Nothing -> IdentNamed entityName
                    Just s -> IdentGenerated (read s)
            qualifiedName -> IdentNamed qualifiedName
