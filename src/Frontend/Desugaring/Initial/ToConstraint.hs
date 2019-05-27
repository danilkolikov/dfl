{- |
Module      :  Frontend.Desugaring.Initial.ToConstraint
Description :  Desugaring of AST nodes to Consrtaint
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Consrtaint-s.
-}
module Frontend.Desugaring.Initial.ToConstraint
    ( DesugarToConstraint(..)
    ) where

import qualified Data.List.NonEmpty as NE (toList)

import Frontend.Desugaring.Initial.Ast
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Desugaring.Initial.ToType (desugarToType)
import Frontend.Syntax.Ast (Class(..))
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Class for types which can be desugared to Constraint
class DesugarToConstraint a where
    desugarToConstraint :: a -> WithLocation Constraint -- ^ Desugar object to constraint

instance (DesugarToConstraint a) => DesugarToConstraint (WithLocation a) where
    desugarToConstraint = (getValue . desugarToConstraint <$>)

instance DesugarToConstraint Class where
    desugarToConstraint (ClassSimple name var) =
        withDummyLocation $
        Constraint (desugarToIdent name) (desugarToIdent var) []
    desugarToConstraint (ClassApplied name var args) =
        withDummyLocation $
        Constraint
            (desugarToIdent name)
            (desugarToIdent var)
            (map desugarToType (NE.toList args))
