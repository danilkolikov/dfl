{- |
Module      :  Frontend.Desugaring.Checking.TypeSynonym
Description :  Disambiguation of type synonyms
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Type for objects which check type synonyms for ambiguity.
-}
module Frontend.Desugaring.Checking.TypeSynonym where

import Frontend.Desugaring.Checking.Base
import Frontend.Desugaring.Checking.Util
import Frontend.Desugaring.Grouping.Ast
import Frontend.Syntax.Position (getValue)

-- | Checks idents in a type synonym for ambiguity
checkTypeSynonym :: TypeSynonym -> CheckingProcessor (Ident, TypeSynonym)
checkTypeSynonym TypeSynonym { getTypeSynonymName = name
                             , getTypeSynonymParams = params
                             , getTypeSynonymType = type'
                             } = do
    checkedName <- checkTypeName name
    checkedType <- checkType type'
    let result =
            TypeSynonym
                { getTypeSynonymName = checkedName
                , getTypeSynonymParams = params
                , getTypeSynonymType = checkedType
                }
    return (getValue checkedName, result)
