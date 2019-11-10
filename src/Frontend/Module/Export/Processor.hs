{- |
Module      :  Frontend.Module.Export.Processor
Description :  Processor for exports
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for processing module exports
-}
module Frontend.Module.Export.Processor
    ( module Frontend.Module.Base
    , processModuleExports
    , ExplicitProcessorError(..)
    ) where

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Processor
import Frontend.Module.Base
import Frontend.Module.Export.Collecting
import Frontend.Module.Export.Explicit
import Frontend.Module.Export.Implicit

-- | Processes exports of a module
processModuleExports ::
       F.Module F.Exp
    -> InferenceProcessorOutput
    -> Either ExplicitProcessorError ModuleExports
processModuleExports module'@F.Module {F.getModuleExports = exports} output = do
    let (allDefinitions, instances) = collectExports module' output
    explicitExports <- selectExplicitExports exports allDefinitions
    let implicitExports =
            selectImplicitExports allDefinitions instances $
            getInferenceProcessorOutputTypeConstructors output
    return
        ModuleExports
            { getModuleExportsExplicit = explicitExports
            , getModuleExportsImplicit = implicitExports
            , getModuleExportsInstances = instances
            }
