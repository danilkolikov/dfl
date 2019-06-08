{- |
Module      :  Frontend.Desugaring.Initial.ToConstraint
Description :  Desugaring of AST nodes to Consrtaint
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Consrtaint-s.
-}
module Frontend.Desugaring.Initial.ToConstraint
    ( desugarToConstraint
    ) where

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE (toList)

import Frontend.Desugaring.Initial.Ast
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Desugaring.Initial.ToType (desugarToType)
import Frontend.Syntax.Ast (Class(..))
import Frontend.Syntax.Position (WithLocation(..))

-- ^ Desugar object to constraint
desugarToConstraint :: WithLocation Class -> WithLocation Constraint
desugarToConstraint cls =
    cls $>
    case getValue cls of
        ClassSimple name var ->
            Constraint (desugarToIdent name) (desugarToIdent var) []
        ClassApplied name var args ->
            Constraint
                (desugarToIdent name)
                (desugarToIdent var)
                (map desugarToType (NE.toList args))
