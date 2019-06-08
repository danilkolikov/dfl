{- |
Module      :  Frontend.Desugaring.Initial.ToNewConstr
Description :  Desugaring of AST nodes to NewConstr
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing NewConstr-s.
-}
module Frontend.Desugaring.Initial.ToNewConstr
    ( desugarToNewConstr
    ) where

import Data.Functor (($>))

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Desugaring.Initial.ToType (desugarToType)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))

-- | Desugar object to a NewConstr
desugarToNewConstr :: WithLocation NewConstr -> WithLocation D.NewConstr
desugarToNewConstr nc =
    nc $>
    case getValue nc of
        NewConstrSimple name type' ->
            D.NewConstrSimple (desugarToIdent name) (desugarToType type')
        NewConstrNamed name fName type' ->
            D.NewConstrRecord
                (desugarToIdent name)
                (desugarToIdent fName)
                (desugarToType type')
