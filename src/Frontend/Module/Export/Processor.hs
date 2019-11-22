{- |
Module      :  Frontend.Module.Export.Processor
Description :  Processor for exports
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for processing module exports
-}
module Frontend.Module.Export.Processor
    ( module Frontend.Module.Base
    , processExports
    , ExplicitProcessorError(..)
    ) where

import Frontend.Base
import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Processor
import Frontend.Module.Base
import Frontend.Module.Export.Collecting
import Frontend.Module.Export.Explicit
import Frontend.Module.Implicit

-- | Processes exports of a module
processExports ::
       FrontendState
    -> ModuleImports
    -> F.Module F.Exp
    -> FrontendState
    -> Either ExplicitProcessorError ModuleExports
processExports importedState imports module' output
    | FrontendState {getFrontendStateInference = importedInferenceOutput} <-
         importedState
    , ModuleImports { getModuleImportsExplicit = importedExplicit
                    , getModuleImportsInstances = importedInstances
                    , getModuleImportsNameMapping = importedMapping
                    } <- imports
    , F.Module {F.getModuleExports = exports} <- module' = do
        let inferenceOutput = getFrontendStateInference output
            (defined, instances) = collectExports module' inferenceOutput
            allDefinitions = importedExplicit <> defined
            allInstances = importedInstances <> instances
            allInferenceOutput = importedInferenceOutput <> inferenceOutput
        explicitExports <-
            selectExplicitExports importedMapping exports allDefinitions
        let implicitExports =
                selectImplicit explicitExports allInstances $
                getInferenceProcessorOutputTypeConstructors allInferenceOutput
        return
            ModuleExports
                { getModuleExportsExplicit = explicitExports
                , getModuleExportsImplicit = implicitExports
                , getModuleExportsInstances = allInstances
                }
