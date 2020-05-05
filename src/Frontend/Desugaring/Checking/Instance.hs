{- |
Module      :  Frontend.Desugaring.Checking.Instance
Description :  Disambiguation of instances
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Type for objects which check instances for ambiguity.
-}
module Frontend.Desugaring.Checking.Instance where

import Frontend.Desugaring.Checking.Base
import Frontend.Desugaring.Checking.Expression
import Frontend.Desugaring.Checking.Util
import Frontend.Desugaring.Grouping.Ast

-- | Checks idents in an instnace for ambiguity
checkInstance :: Instance Exp -> CheckingProcessor (Instance Exp)
checkInstance Instance { getInstanceContext = context
                       , getInstanceClass = className
                       , getInstanceType = typeName
                       , getInstanceTypeArgs = args
                       , getInstanceMethods = exps
                       } = do
    checkedContext <- mapM checkSimpleConstraint context
    checkedClassName <- checkTypeName className
    checkedTypeName <- checkTypeName typeName
    checkedMethods <- checkExps exps
    return
        Instance
            { getInstanceContext = checkedContext
            , getInstanceClass = checkedClassName
            , getInstanceType = checkedTypeName
            , getInstanceTypeArgs = args
            , getInstanceMethods = checkedMethods
            }
