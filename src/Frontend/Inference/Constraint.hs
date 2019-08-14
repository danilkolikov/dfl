{- |
Module      :  Frontend.Inference.Constraint
Description :  Definition of a constraint
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with the definition of a type constraint
-}
module Frontend.Inference.Constraint where

import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Final.Ast (Ident)
import Frontend.Inference.Type
import Frontend.Inference.WithVariables

-- | A type constraint
data Constraint
    = ConstraintVariable { getConstraintClass :: Ident -- ^ A class name
                         , getConstraintVariable :: Type -- ^ A constrained variable
                          } -- ^ A constrained type variable
    | ConstraintType { getConstraintClass :: Ident -- ^ A class name
                     , getConstraintType :: Ident -- ^ A type name
                     , getConstraintTypeArgs :: NE.NonEmpty Type -- ^ Type aguments
                      } -- ^ A constrained data type
    deriving (Eq, Show)

instance WithVariables Constraint where
    getVariableName _ = Nothing
    getFreeVariables constr =
        case constr of
            ConstraintVariable {getConstraintVariable = var} ->
                getFreeVariables var
            ConstraintType {getConstraintTypeArgs = args} ->
                HS.unions . NE.toList . fmap getFreeVariables $ args

-- | A constraint defining type class hierarchy
data SimpleConstraint = SimpleConstraint
    { getSimpleConstraintClass :: Ident -- ^ A class name
    , getSimpleConstraintVariable :: Ident -- ^ A constrained variable
    } deriving (Eq, Show)
