{- |
Module      :  Frontend.Desugaring.Checking.DataType
Description :  Disambiguation of data types
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Type for objects which check data types for ambiguity.
-}
module Frontend.Desugaring.Checking.DataType where

import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Checking.Base
import Frontend.Desugaring.Checking.Util
import Frontend.Desugaring.Grouping.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Checks idents in a data type for ambiguity
checkDataType :: DataType -> CheckingProcessor (Ident, DataType)
checkDataType DataType { getDataTypeContext = context
                       , getDataTypeName = name
                       , getDataTypeParams = params
                       , getDataTypeDeriving = deriving'
                       , getDataTypeConstructors = constructors
                       , isNewType = newType
                       } = do
    checkedContext <- mapM checkConstraint context
    checkedName <- checkTypeName name
    checkedDeriving <- mapM checkTypeName deriving'
    checkedConstructors <- mapM checkConstructor constructors
    let result =
            DataType
                { getDataTypeContext = checkedContext
                , getDataTypeName = checkedName
                , getDataTypeParams = params
                , getDataTypeDeriving = checkedDeriving
                , getDataTypeConstructors = checkedConstructors
                , isNewType = newType
                }
    return (getValue checkedName, result)

-- | Checks idents in a constructor for ambiguity
checkConstructor ::
       (Ident, Constructor) -> CheckingProcessor (Ident, Constructor)
checkConstructor (_, Constructor { getConstructorName = name
                                 , getConstructorArgs = args
                                 , getConstructorFields = fields
                                 }) = do
    checkedName <- checkExpressionName name
    checkedArgs <- mapM checkType args
    let checkField (fieldName, pos) = do
            checkedFieldName <-
                checkExpressionName (withDummyLocation fieldName)
            return (getValue checkedFieldName, pos)
    checkedFields <- HM.fromList <$> mapM checkField (HM.toList fields)
    return
        ( getValue checkedName
        , Constructor
              { getConstructorName = checkedName
              , getConstructorArgs = checkedArgs
              , getConstructorFields = checkedFields
              })
