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

import qualified Data.List.NonEmpty as NE (NonEmpty(..))

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Syntax.Ast
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position
    ( SourceLocation(..)
    , WithLocation(..)
    , withDummyLocation
    )

-- | Class of types which can be desugared to Type
class DesugarToType a where
    desugarToType :: a -> WithLocation D.Type -- ^ Desugar object to Type

instance (DesugarToType a) => DesugarToType (WithLocation a) where
    desugarToType = (getValue . desugarToType <$>)

instance DesugarToType Type where
    desugarToType (Type (f NE.:| rest)) =
        let ident = D.IdentNamed fUNCTION_NAME
            func = withDummyLocation (D.TypeConstr (withDummyLocation ident))
            desugaredFirst = desugarToType f
         in case rest of
                [] -> desugaredFirst
                (t:ts) ->
                    let startLocation = getLocationStart . getLocation $ t
                        endLocation =
                            getLocationEnd . getLocation . last $ rest
                        restType = Type (t NE.:| ts)
                        restLocation = SourceLocation startLocation endLocation
                        desugaredRest =
                            desugarToType (WithLocation restType restLocation)
                     in withDummyLocation
                            (D.TypeApplication
                                 func
                                 (desugaredFirst NE.:| [desugaredRest]))

instance DesugarToType BType where
    desugarToType (BType (f NE.:| rest)) =
        let func = desugarToType f
            desugared = map desugarToType rest
         in case desugared of
                [] -> func
                (t:ts) ->
                    withDummyLocation $ D.TypeApplication func (t NE.:| ts)

instance DesugarToType AType where
    desugarToType (ATypeConstructor name) =
        withDummyLocation . D.TypeConstr . desugarToIdent $ name
    desugarToType (ATypeVar name) =
        withDummyLocation . D.TypeVar . desugarToIdent $ name
    desugarToType (ATypeTuple f s rest) =
        let ident =
                wrapIdentToType $
                D.IdentParametrised tUPLE_NAME (length rest + 2)
            args = f NE.:| (s : rest)
            desugaredArgs = fmap desugarToType args
         in withDummyLocation $ D.TypeApplication ident desugaredArgs
    desugarToType (ATypeList t) =
        let ident = wrapIdentToType $ D.IdentNamed lIST_NAME
            args = t NE.:| []
            desugaredArgs = fmap desugarToType args
         in withDummyLocation $ D.TypeApplication ident desugaredArgs
    desugarToType (ATypeParens t) = desugarToType t

-- Helper functions
wrapIdentToType :: D.Ident -> WithLocation D.Type
wrapIdentToType = withDummyLocation . D.TypeConstr . withDummyLocation
