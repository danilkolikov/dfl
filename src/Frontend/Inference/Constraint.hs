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

import qualified Frontend.Desugaring.Final.Ast as F
    ( Constraint(..)
    , Ident
    , SimpleConstraint(..)
    )
import Frontend.Inference.Type
import Frontend.Inference.WithVariables
import Frontend.Syntax.Position (WithLocation(..))

-- | A type constraint
data Constraint
    = ConstraintVariable { getConstraintClass :: F.Ident -- ^ A class name
                         , getConstraintVariable :: Type -- ^ A constrained variable
                          } -- ^ A constrained type variable
    | ConstraintAppliedVariable { getConstraintClass :: F.Ident -- ^ A class name
                                , getConstraintVariable :: Type -- ^ A constrained variable
                                , getConstraintArgs :: NE.NonEmpty Type -- ^ Type aguments
                                 } -- ^ A constrained data type
    deriving (Eq, Show)

instance WithVariables Constraint where
    getVariableName _ = Nothing
    getFreeVariables constr =
        case constr of
            ConstraintVariable {getConstraintVariable = var} ->
                getFreeVariables var
            ConstraintAppliedVariable { getConstraintVariable = var
                                      , getConstraintArgs = args
                                      } ->
                HS.unions $
                getFreeVariables var : NE.toList (fmap getFreeVariables $ args)

-- | A constraint defining type class hierarchy
data SimpleConstraint = SimpleConstraint
    { getSimpleConstraintClass :: F.Ident -- ^ A class name
    , getSimpleConstraintVariable :: F.Ident -- ^ A constrained variable
    } deriving (Eq, Show)

-- | Drops information about positions
removePositionsOfConstraint :: WithLocation F.Constraint -> Constraint
removePositionsOfConstraint constraint =
    case getValue constraint of
        F.ConstraintParam className param ->
            ConstraintVariable (getValue className) (TypeVar $ getValue param)
        F.ConstraintAppliedParam className paramName args ->
            ConstraintAppliedVariable
                (getValue className)
                (TypeVar $ getValue paramName)
                (fmap removePositionsOfType args)

-- | Converts simple constraints
removePositionsOfSimpleConstraint ::
       WithLocation F.SimpleConstraint -> SimpleConstraint
removePositionsOfSimpleConstraint sc
    | F.SimpleConstraint name param <- getValue sc =
        SimpleConstraint (getValue name) (getValue param)
