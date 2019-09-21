{- |
Module      :  Frontend.Inference.Constraint
Description :  Definition of a constraint
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with the definition of a type constraint
-}
module Frontend.Inference.Constraint where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)

import Frontend.Desugaring.Final.Ast (Ident)
import qualified Frontend.Desugaring.Final.Ast as F
    ( Constraint(..)
    , SimpleConstraint(..)
    )
import Frontend.Inference.Substitution
import Frontend.Inference.Type
import Frontend.Inference.WithVariables
import Frontend.Syntax.Position (WithLocation(..))

-- | A type constraint
data Constraint
    = ConstraintVariable { getConstraintClass :: Ident -- ^ A class name
                         , getConstraintVariable :: Ident -- ^ A constrained variable
                          } -- ^ A constrained type variable
    | ConstraintAppliedVariable { getConstraintClass :: Ident -- ^ A class name
                                , getConstraintVariable :: Ident -- ^ A constrained variable
                                , getConstraintArgs :: NE.NonEmpty Type -- ^ Type aguments
                                 } -- ^ A constrained data type
    deriving (Eq, Show)

instance WithVariables Constraint where
    getVariableName _ = Nothing
    getFreeVariables constr =
        case constr of
            ConstraintVariable {getConstraintVariable = var} -> HS.singleton var
            ConstraintAppliedVariable { getConstraintVariable = var
                                      , getConstraintArgs = args
                                      } ->
                HS.unions $
                HS.singleton var : NE.toList (fmap getFreeVariables args)

-- | A constraint defining type class hierarchy
data SimpleConstraint = SimpleConstraint
    { getSimpleConstraintClass :: Ident -- ^ A class name
    , getSimpleConstraintVariable :: Ident -- ^ A constrained variable
    } deriving (Eq, Show)

-- | Drops information about positions
removePositionsOfConstraint :: WithLocation F.Constraint -> Constraint
removePositionsOfConstraint constraint =
    case getValue constraint of
        F.ConstraintParam className param ->
            ConstraintVariable (getValue className) (getValue param)
        F.ConstraintAppliedParam className paramName args ->
            ConstraintAppliedVariable
                (getValue className)
                (getValue paramName)
                (fmap removePositionsOfType args)

-- | Converts simple constraints
removePositionsOfSimpleConstraint ::
       WithLocation F.SimpleConstraint -> SimpleConstraint
removePositionsOfSimpleConstraint sc
    | F.SimpleConstraint name param <- getValue sc =
        SimpleConstraint (getValue name) (getValue param)

-- | Substitutes variables using the provided map
substituteVariables :: Substitution Ident -> Constraint -> Constraint
substituteVariables sub constraint =
    let makeNewVar var = fromMaybe var (HM.lookup var sub)
        typeSub = HM.map TypeVar sub
     in case constraint of
            ConstraintVariable cls var ->
                ConstraintVariable cls $ makeNewVar var
            ConstraintAppliedVariable cls var args ->
                ConstraintAppliedVariable
                    cls
                    (makeNewVar var)
                    (fmap (substitute typeSub) args)

-- | Substitutes variables in a constraint and returns a result, only if it's valid
substituteConstraint :: Substitution Type -> Constraint -> Maybe Constraint
substituteConstraint sub constr =
    case constr of
        ConstraintVariable cls var ->
            case HM.lookup var sub of
                Nothing -> Just constr
                Just t ->
                    case t of
                        TypeVar newVar -> Just $ ConstraintVariable cls newVar
                        TypeConstr _ -> Nothing -- Not a valid constraint
                        TypeFunction _ _ -> Nothing -- Not a valid constraint,
                                                    -- because it's an application of a "constructor"
                        TypeApplication func args ->
                            processApplication cls func args
        ConstraintAppliedVariable cls var args ->
            case HM.lookup var sub of
                Nothing -> Just constr
                Just t -> processApplication cls t args
  where
    processApplication :: Ident -> Type -> NE.NonEmpty Type -> Maybe Constraint
    processApplication cls type' args =
        case type' of
            TypeVar newVar -> Just $ ConstraintAppliedVariable cls newVar args
            TypeConstr _ -> Nothing -- Not a valid constraint
            TypeFunction _ _ -> Nothing -- Not a valid constraint,
                                        -- because it's an application of a "constructor"
            TypeApplication func (first NE.:| rest) ->
                processApplication cls func (first NE.:| rest ++ NE.toList args)
