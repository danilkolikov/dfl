{- |
Module      :  Frontend.Desugaring.Grouping.Processor
Description :  Grouping of modules
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Grouping of modules
-}
module Frontend.Desugaring.Grouping.Processor
    ( processModule
    , GroupingProcessorError(..)
    , GroupingProcessorState(..)
    , GroupingDebugOutput(..)
    ) where

import Frontend.Desugaring.Grouping.Assignment (groupTopLevelAssignments)
import Frontend.Desugaring.Grouping.Ast
import Frontend.Desugaring.Grouping.Base
import Frontend.Desugaring.Grouping.Class (groupClasses)
import Frontend.Desugaring.Grouping.DataType (groupDataTypes)
import Frontend.Desugaring.Grouping.Instance (groupInstances)
import Frontend.Desugaring.Grouping.TypeSynonym (groupTypeSynonyms)
import Frontend.Desugaring.Grouping.Util
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position

-- | Groups expressions in a module
processModule ::
       I.Module
    -> ( Either GroupingProcessorError (Module Exp, GroupingProcessorState)
       , GroupingDebugOutput)
processModule module'@(I.Module name _ _ _) =
    let moduleName = getValue $ wrapIdent name
     in runGroupingProcessor moduleName (groupModule module')

-- | Groups expressions in a module
groupModule :: I.Module -> GroupingProcessor (Module Exp)
groupModule (I.Module name exports _ decls) = do
    typeSynonyms <- groupTypeSynonyms decls
    writeGroupingDebugOutput
        mempty {getGroupingDebugOutputTypeSynonyms = Just typeSynonyms}
    dataTypes <- groupDataTypes decls
    writeGroupingDebugOutput
        mempty {getGroupingDebugOutputDataTypes = Just dataTypes}
    classes <- groupClasses decls
    writeGroupingDebugOutput
        mempty {getGroupingDebugOutputClassses = Just classes}
    instances <- groupInstances decls
    writeGroupingDebugOutput
        mempty {getGroupingDebugOutputInstances = Just instances}
    exprs <- groupTopLevelAssignments decls
    writeGroupingDebugOutput
        mempty {getGroupingDebugOutputFunctions = Just exprs}
    return
        Module
            { getModuleName = wrapIdent name
            , getModuleExports = fmap wrapExport exports
            , getModuleTypeSynonyms = typeSynonyms
            , getModuleDataTypes = dataTypes
            , getModuleClasses = classes
            , getModuleInstances = instances
            , getModuleExpressions = exprs
            }
