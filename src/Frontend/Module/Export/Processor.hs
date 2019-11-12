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
       F.Module F.Exp
    -> FrontendState
    -> Either ExplicitProcessorError ModuleExports
processExports module'@F.Module {F.getModuleExports = exports} output = do
    let inferenceOutput = getFrontendStateInference output
        (allDefinitions, instances) = collectExports module' inferenceOutput
    explicitExports <- selectExplicitExports exports allDefinitions
    let implicitExports =
            selectImplicit allDefinitions instances $
            getInferenceProcessorOutputTypeConstructors inferenceOutput
    return
        ModuleExports
            { getModuleExportsExplicit = explicitExports
            , getModuleExportsImplicit = implicitExports
            , getModuleExportsInstances = instances
            }
