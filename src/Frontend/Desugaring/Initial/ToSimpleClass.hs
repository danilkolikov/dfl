{- |
Module      :  Frontend.Desugaring.Initial.ToSimpleClass
Description :  Desugaring of AST nodes to SimpleClass
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing SimpleClass-s.
-}
module Frontend.Desugaring.Initial.ToSimpleClass
    ( desugarToSimpleClass
    ) where

import Data.Functor (($>))

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))

-- | Desugar object to SimpleClass
desugarToSimpleClass :: WithLocation SimpleClass -> WithLocation D.SimpleClass
desugarToSimpleClass sc
    | SimpleClass name var <- getValue sc =
        sc $> D.SimpleClass (desugarToIdent name) (desugarToIdent var)
