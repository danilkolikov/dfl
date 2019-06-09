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

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE (NonEmpty(..))

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Desugaring.Initial.Util
import Frontend.Syntax.Ast
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..))

-- | Class of types which can be desugared to Type
class DesugarToType a where
    desugarToType :: WithLocation a -> WithLocation D.Type -- ^ Desugar object to Type

instance DesugarToType Type where
    desugarToType type'
        | Type (f NE.:| rest) <- getValue type' =
            let func = makeTypeConstr fUNCTION_NAME
                desugaredFirst = desugarToType f
             in case rest of
                    [] -> desugaredFirst
                    t:ts ->
                        let restType = type' $> Type (t NE.:| ts)
                            desugaredRest = desugarToType restType
                         in type' $>
                            D.TypeApplication
                                func
                                (desugaredFirst NE.:| [desugaredRest])

instance DesugarToType BType where
    desugarToType bType
        | BType (f NE.:| rest) <- getValue bType =
            let func = desugarToType f
                desugared = map desugarToType rest
             in case desugared of
                    [] -> func
                    t:ts -> bType $> D.TypeApplication func (t NE.:| ts)

instance DesugarToType AType where
    desugarToType aType =
        aType $>
        case getValue aType of
            ATypeConstructor name -> D.TypeConstr . desugarToIdent $ name
            ATypeVar name -> D.TypeVar . desugarToIdent $ name
            ATypeTuple f s rest ->
                let ident =
                        makeTypeConstr' $
                        D.IdentParametrised tUPLE_NAME (length rest + 2)
                    args = f NE.:| (s : rest)
                    desugaredArgs = fmap desugarToType args
                 in D.TypeApplication ident desugaredArgs
            ATypeList t ->
                let ident = makeTypeConstr lIST_NAME
                    args = t NE.:| []
                    desugaredArgs = fmap desugarToType args
                 in D.TypeApplication ident desugaredArgs
            ATypeParens t -> getValue $ desugarToType t
