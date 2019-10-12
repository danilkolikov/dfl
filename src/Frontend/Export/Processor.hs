{- |
Module      :  Frontend.Export.Processor
Description :  Processor for exports
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for processing module exports
-}
module Frontend.Export.Processor
    ( ModuleExports(..)
    , emptyModuleExports
    , ImplicitExport(..)
    , module Frontend.Export.Ast
    , processModuleExports
    ) where

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Export.Ast
import Frontend.Export.Collecting
import Frontend.Export.Explicit
import Frontend.Export.Implicit
import Frontend.Inference.Processor

-- | All exports of a module
data ModuleExports = ModuleExports
    { getModuleExportsExplicit :: Module
    , getModuleExportsImplicit :: ImplicitExport
    } deriving (Eq, Show)

-- | Empty exports
emptyModuleExports :: ModuleExports
emptyModuleExports = ModuleExports mempty emptyImplicitExport

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
