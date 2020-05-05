{- |
Module      :  Frontend.Desugaring.Checking.Class
Description :  Disambiguation of classes
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Type for objects which check classes for ambiguity.
-}
module Frontend.Desugaring.Checking.Class where

import Frontend.Desugaring.Checking.Base
import Frontend.Desugaring.Checking.Expression
import Frontend.Desugaring.Checking.Util
import Frontend.Desugaring.Grouping.Ast
import Frontend.Syntax.Position (getValue)

-- | Checks idents in a class for ambiguity
checkClass :: Class Exp -> CheckingProcessor (Ident, Class Exp)
checkClass Class { getClassContext = context
                 , getClassName = name
                 , getClassParam = param
                 , getClassMethods = methods
                 } = do
    checkedContext <- mapM checkSimpleConstraint context
    checkedName <- checkTypeName name
    checkedMethods <- checkMethods methods
    let result =
            Class
                { getClassContext = checkedContext
                , getClassName = checkedName
                , getClassParam = param
                , getClassMethods = checkedMethods
                }
    return (getValue checkedName, result)
