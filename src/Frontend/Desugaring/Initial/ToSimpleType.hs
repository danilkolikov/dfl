{- |
Module      :  Frontend.Desugaring.Initial.ToSimpleType
Description :  Desugaring of AST nodes to SimpleType
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing SimpleType-s.
-}
module Frontend.Desugaring.Initial.ToSimpleType
    ( desugarToSimpleType
    ) where

import Data.Functor (($>))

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))

-- | Desugar object to SimpleType
desugarToSimpleType :: WithLocation SimpleType -> WithLocation D.SimpleType
desugarToSimpleType st
    | SimpleType con vars <- getValue st =
        st $> D.SimpleType (desugarToIdent con) (map desugarToIdent vars)
