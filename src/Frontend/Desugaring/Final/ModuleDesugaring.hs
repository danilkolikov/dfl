{- |
Module      :  Frontend.Desugaring.Final.ModuleDesugaring
Description :  Final desugaring of modules
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of modules
-}
module Frontend.Desugaring.Final.ModuleDesugaring
    ( desugarModule
    ) where

import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Final.AssignmentDesugaring
    ( desugarTopLevelAssignments
    , desugarFieldGetters
    )
import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.ClassDesugaring (desugarClasses)
import Frontend.Desugaring.Final.DataTypeDesugaring (desugarDataTypes)
import Frontend.Desugaring.Final.InstanceDesugaring (desugarInstances)
import Frontend.Desugaring.Final.Processor
import Frontend.Desugaring.Final.TypeSynonymDesugaring (desugarTypeSynonyms)
import qualified Frontend.Desugaring.Initial.Ast as I

-- | Desugar module
desugarModule :: I.Module -> DesugaringProcessor Module
desugarModule (I.Module name exports imports decls) = do
    dataTypes <- desugarDataTypes decls
    getters <- desugarFieldGetters dataTypes
    typeSynonyms <- desugarTypeSynonyms decls
    classes <- desugarClasses decls
    instances <- desugarInstances decls
    exprs <- desugarTopLevelAssignments decls
    return
        Module
            { getModuleName = name
            , getModuleExports = exports
            , getModuleImports = imports
            , getModuleTypeSynonyms = typeSynonyms
            , getModuleDataTypes = dataTypes
            , getModuleClasses = classes
            , getModuleInstances = instances
            , getModuleExpressions = HM.union getters exprs
            }
