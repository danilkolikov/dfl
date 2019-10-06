{- |
Module      :  Frontend.Desugaring.Checking.Util
Description :  Utility functions for disambiguation
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Utility functions for ambiguity checks.
-}
module Frontend.Desugaring.Checking.Util where

import Control.Monad (liftM2)
import Data.Functor (($>))

import Frontend.Desugaring.Checking.Base
import Frontend.Desugaring.Grouping.Ast
import Frontend.Syntax.Position (WithLocation(..))

-- | Checks idents in a type for ambiguity
checkType :: WithLocation Type -> CheckingProcessor (WithLocation Type)
checkType t =
    (t $>) <$>
    case getValue t of
        TypeVar name -> return $ TypeVar name
        TypeConstr name -> TypeConstr <$> checkTypeName name
        TypeFunction from to ->
            liftM2 TypeFunction (checkType from) (checkType to)
        TypeApplication func args ->
            liftM2 TypeApplication (checkType func) (mapM checkType args)

-- | Checks idents in a type signature for ambiguity
checkTypeSignature :: TypeSignature -> CheckingProcessor TypeSignature
checkTypeSignature TypeSignature { getTypeSignatureContext = context
                                 , getTypeSignatureType = type'
                                 } = do
    checkedContext <- mapM checkConstraint context
    checkedType <- checkType type'
    return
        TypeSignature
            { getTypeSignatureContext = checkedContext
            , getTypeSignatureType = checkedType
            }

-- | Checks idents in a constraint
checkConstraint ::
       WithLocation Constraint -> CheckingProcessor (WithLocation Constraint)
checkConstraint c =
    (c $>) <$>
    case getValue c of
        ConstraintParam className param ->
            (`ConstraintParam` param) <$> checkTypeName className
        ConstraintAppliedParam className param args ->
            liftM2
                (\cl a -> ConstraintAppliedParam cl param a)
                (checkTypeName className)
                (mapM checkType args)

-- | Checks idents in a simple class
checkSimpleConstraint ::
       WithLocation SimpleConstraint
    -> CheckingProcessor (WithLocation SimpleConstraint)
checkSimpleConstraint c =
    (c $>) <$>
    case getValue c of
        SimpleConstraint name param ->
            (`SimpleConstraint` param) <$> checkTypeName name
