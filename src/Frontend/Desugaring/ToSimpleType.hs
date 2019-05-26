{- |
Module      :  Frontend.Desugaring.ToSimpleType
Description :  Desugaring of AST nodes to SimpleType
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing SimpleType-s.
-}
module Frontend.Desugaring.ToSimpleType where

import qualified Frontend.Desugaring.Ast as D
import Frontend.Desugaring.ToIdent (desugarToIdent)
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
