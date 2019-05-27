{- |
Module      :  Frontend.Desugaring.Initial.ToType
Description :  Desugaring of AST nodes to Type
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Type-s.
-}
module Frontend.Desugaring.Initial.ToType
    ( DesugarToType(..)
    ) where

import qualified Data.List.NonEmpty as NE (NonEmpty(..), init, last)

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Syntax.Ast
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Class of types which can be desugared to Type
class DesugarToType a where
    desugarToType :: a -> WithLocation D.Type -- ^ Desugar object to Type

instance (DesugarToType a) => DesugarToType (WithLocation a) where
    desugarToType = (getValue . desugarToType <$>)

instance DesugarToType Type where
    desugarToType (Type args) =
        let ident = D.IdentNamed fUNCTION_NAME
            func = withDummyLocation (D.TypeConstr (withDummyLocation ident))
         in foldr
                (\f s ->
                     withDummyLocation
                         (D.TypeApplication func (desugarToType f NE.:| [s])))
                (desugarToType $ NE.last args)
                (NE.init args)

instance DesugarToType BType where
    desugarToType (BType (f NE.:| rest)) =
        let func = desugarToType f
         in case rest of
                [] -> func
                (t:ts) ->
                    withDummyLocation $
                    D.TypeApplication
                        func
                        (desugarToType t NE.:| map desugarToType ts)

instance DesugarToType AType where
    desugarToType (ATypeConstructor name) =
        withDummyLocation . D.TypeConstr . desugarToIdent $ name
    desugarToType (ATypeVar name) =
        withDummyLocation . D.TypeVar . desugarToIdent $ name
    desugarToType (ATypeTuple f s rest) =
        let ident =
                wrapIdentToType $
                D.IdentParametrised tUPLE_NAME (length rest + 2)
         in withDummyLocation $
            D.TypeApplication
                ident
                (desugarToType f NE.:| map desugarToType (s : rest))
    desugarToType (ATypeList t) =
        let ident = wrapIdentToType $ D.IdentNamed lIST_NAME
         in withDummyLocation $
            D.TypeApplication ident (desugarToType t NE.:| [])
    desugarToType (ATypeParens t) = desugarToType t

-- Helper functions
wrapIdentToType :: D.Ident -> WithLocation D.Type
wrapIdentToType = withDummyLocation . D.TypeConstr . withDummyLocation
