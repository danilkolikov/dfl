{- |
Module      :  Frontend.Desugaring.Initial.ToIdent
Description :  Desugaring of AST nodes to Ident
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Ident-s.
-}
module Frontend.Desugaring.Initial.ToIdent
    ( DesugarToIdent(..)
    ) where

import Data.Functor (($>))

import Frontend.Desugaring.Initial.Ast
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
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Syntax.Token (ConId, ConSym, VarId, VarSym)

-- | Class for types which can be desugared to Ident-s
class DesugarToIdent a where
    desugarToIdent :: WithLocation a -> WithLocation Ident -- ^ Desugar object to Ident

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
    desugarToIdent gCon =
        gCon $>
        case getValue gCon of
            GConNamed name -> getValue $ desugarToIdent name
            GConUnit -> IdentNamed uNIT_NAME
            GConList -> IdentNamed lIST_NAME
            GConTuple n -> IdentParametrised tUPLE_NAME n

instance DesugarToIdent GTyCon where
    desugarToIdent gTyCon =
        gTyCon $>
        case getValue gTyCon of
            GTyConNamed name -> getValue $ desugarToIdent name
            GTyConUnit -> IdentNamed uNIT_NAME
            GTyConList -> IdentNamed lIST_NAME
            GTyConTuple n -> IdentParametrised tUPLE_NAME n
            GTyConFunction -> IdentNamed fUNCTION_NAME

instance DesugarToIdent DClass where
    desugarToIdent dClass
        | DClass name <- getValue dClass = desugarToIdent name

-- Helper functions
desugarNamedIdent :: (NamedEntity a) => WithLocation a -> WithLocation Ident
desugarNamedIdent (WithLocation ident loc) =
    WithLocation (IdentNamed $ getEntityName ident) loc
