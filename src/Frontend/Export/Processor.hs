{- |
Module      :  Frontend.Export.Processor
Description :  Processor for exports
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for processing module exports
-}
module Frontend.Export.Processor
    ( module Frontend.Export.Base
    , processModuleExports
    ) where

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Export.Base
import Frontend.Export.Collecting
import Frontend.Export.Explicit
import Frontend.Export.Implicit
import Frontend.Inference.Processor

-- | Processes exports of a module
processModuleExports ::
       F.Module F.Exp -> InferenceProcessorOutput -> ModuleExports
processModuleExports module'@F.Module {F.getModuleExports = exports} output =
    let allDefinitions = collectExports module' output
        explicitExports = selectExplicitExports exports allDefinitions
        implicitExports =
            selectImplicitExports allDefinitions $
            getInferenceProcessorOutputTypeConstructors output
     in ModuleExports
            { getModuleExportsExplicit = explicitExports
            , getModuleExportsImplicit = implicitExports
            }
