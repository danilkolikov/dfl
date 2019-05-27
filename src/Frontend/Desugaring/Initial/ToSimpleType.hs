{- |
Module      :  Frontend.Desugaring.Initial.ToSimpleType
Description :  Desugaring of AST nodes to SimpleType
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing SimpleType-s.
-}
module Frontend.Desugaring.Initial.ToSimpleType where

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Class for types which can be desugared to SimpleType
class DesugarToSimpleType a where
    desugarToSimpleType :: a -> WithLocation D.SimpleType -- ^ Desugar object to SimpleType

instance (DesugarToSimpleType a) => DesugarToSimpleType (WithLocation a) where
    desugarToSimpleType = (getValue . desugarToSimpleType <$>)

instance DesugarToSimpleType SimpleType where
    desugarToSimpleType (SimpleType con vars) =
        withDummyLocation $
        D.SimpleType (desugarToIdent con) (map desugarToIdent vars)
