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

import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Grouping.Assignment
    ( ensureEmptyFixities
    , groupTopLevelAssignments
    )
import Frontend.Desugaring.Grouping.Ast
import Frontend.Desugaring.Grouping.Base
import Frontend.Desugaring.Grouping.Class
    ( addFixityToClassMethods
    , groupClasses
    )
import Frontend.Desugaring.Grouping.DataType
    ( addFixityToDataTypeConstructors
    , groupDataTypes
    )
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
    (exprs, fixities) <- groupTopLevelAssignments decls
    writeGroupingDebugOutput
        mempty {getGroupingDebugOutputFunctions = Just exprs}
    -- Set fixity signatures to data types and class methods
    let classesWithFixity = HM.map (addFixityToClassMethods fixities) classes
        dataTypesWithFixity =
            HM.map (addFixityToDataTypeConstructors fixities) dataTypes
        remainingFixities =
            getRemainingFixities classesWithFixity dataTypesWithFixity fixities
    -- Raise an error if not all signatures were used
    ensureEmptyFixities remainingFixities
    return
        Module
            { getModuleName = wrapIdent name
            , getModuleExports = fmap wrapExport exports
            , getModuleTypeSynonyms = typeSynonyms
            , getModuleDataTypes = dataTypesWithFixity
            , getModuleClasses = classesWithFixity
            , getModuleInstances = instances
            , getModuleExpressions = exprs
            }

-- | Selects unused fixity signatures
getRemainingFixities ::
       Classes Exp
    -> DataTypes
    -> HM.HashMap Ident FixitySignature
    -> HM.HashMap Ident FixitySignature
getRemainingFixities classes dataTypes fixities =
    let methods = HM.unions . map getClassMethods $ HM.elems classes
        constructors =
            HM.unions . map (HM.fromList . getDataTypeConstructors) $
            HM.elems dataTypes
     in fixities `HM.difference` methods `HM.difference` constructors
