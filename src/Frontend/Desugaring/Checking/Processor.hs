{- |
Module      :  Frontend.Desugaring.Checking.Processor
Description :  Disambiguation of type synonyms
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for ambiguity checking.
-}
module Frontend.Desugaring.Checking.Processor
    ( checkModule
    , CheckingError(..)
    , CheckingDebugOutput(..)
    ) where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Frontend.Desugaring.Checking.Base
import Frontend.Desugaring.Checking.Class
import Frontend.Desugaring.Checking.DataType
import Frontend.Desugaring.Checking.Export
import Frontend.Desugaring.Checking.Expression
import Frontend.Desugaring.Checking.Instance
import Frontend.Desugaring.Checking.TypeSynonym
import Frontend.Desugaring.Grouping.Ast
import Frontend.Desugaring.Grouping.Base
import Frontend.Syntax.Position

-- | Checks idents in a module for ambiguity
checkModule ::
       ImportedGroups
    -> GroupingProcessorState
    -> Module Exp
    -> (Either CheckingError (Module Exp), CheckingDebugOutput)
checkModule imports topLevel module' =
    let environment =
            CheckingProcessorEnvironment
                { getCheckingProcessorEnvironmentModuleName =
                      getValue $ getModuleName module'
                , getCheckingProcessorEnvironmentImports = imports
                , getCheckingProcessorEnvironmentTopLevel = topLevel
                , getCheckingProcessorEnvironmentLocalDefinitions = HS.empty
                }
     in runCheckingProcessor environment (checkModule' module')

checkModule' :: Module Exp -> CheckingProcessor (Module Exp)
checkModule' Module { getModuleName = name
                    , getModuleExports = exports
                    , getModuleDataTypes = dataTypes
                    , getModuleTypeSynonyms = typeSynonyms
                    , getModuleClasses = classes
                    , getModuleInstances = instances
                    , getModuleExpressions = expressions
                    } = do
    checkedExports <- traverse checkExport exports
    writeCheckingDebugOutput
        mempty {getCheckingDebugOutputExports = Just checkedExports}
    checkedDataTypes <- checkMap checkDataType dataTypes
    writeCheckingDebugOutput
        mempty {getCheckingDebugOutputDataTypes = Just checkedDataTypes}
    checkedTypeSynonyms <- checkMap checkTypeSynonym typeSynonyms
    writeCheckingDebugOutput
        mempty {getCheckingDebugOutputTypeSynonyms = Just checkedTypeSynonyms}
    checkedClasses <- checkMap checkClass classes
    writeCheckingDebugOutput
        mempty {getCheckingDebugOutputClassses = Just checkedClasses}
    checkedInstances <- mapM checkInstance instances
    writeCheckingDebugOutput
        mempty {getCheckingDebugOutputInstances = Just checkedInstances}
    checkedExpressions <- checkExpressions expressions
    writeCheckingDebugOutput
        mempty {getCheckingDebugOutputFunctions = Just checkedExpressions}
    return
        Module
            { getModuleName = name
            , getModuleExports = checkedExports
            , getModuleDataTypes = checkedDataTypes
            , getModuleTypeSynonyms = checkedTypeSynonyms
            , getModuleClasses = checkedClasses
            , getModuleInstances = checkedInstances
            , getModuleExpressions = checkedExpressions
            }

checkMap ::
       (a -> CheckingProcessor (Ident, a))
    -> HM.HashMap Ident a
    -> CheckingProcessor (HM.HashMap Ident a)
checkMap checker m = HM.fromList <$> mapM checker (HM.elems m)
