{- |
Module      :  Compiler.Module.Processor
Description :  Processor of multiple modules
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of compilation of DFL files
-}
module Compiler.Module.Processor
    ( compileMultipleModules
    ) where

import qualified Data.HashSet as HS

import Compiler.Base
import Compiler.Module.Base
import Compiler.Module.Dependencies
import Compiler.Prettify.ModuleExports ()
import Frontend.HeaderProcessor
import Frontend.Module.Export.Processor
import Frontend.Module.Import.Processor
import Frontend.Processor
import Util.DependencyResolver

-- | Finds dependencies between modules and compiles them
compileMultipleModules :: (Compiler m) => m ()
compileMultipleModules = do
    dependencies <- resolveDependencies
    (_, compilation) <-
        handleResult $
        traverseGraph compileModule emptyDefinedModules dependencies
    _ <- compilation
    return ()

compileModule ::
       (Compiler m) => DefinedModules -> HS.HashSet FileName -> m DefinedModules
compileModule initialState group = do
    let fileName = getFileName . head $ HS.toList group -- Each group consists of only 1 module
    fileContent <- readFileContent fileName
    -- Read header
    HeaderProcessorOutput { getHeaderProcessorOutputTokens = tokens
                          , getHeaderProcessorOutputHeader = header
                          } <-
        traceStepWithDebugOutput (fileName ++ ".header") $
        processModuleHeader fileName fileContent
    -- Select imported declarations and compile
    moduleImports <- handleResult $ processImports initialState header
    FrontendProcessorOutput { getFrontendProcessorOutputExpressions = expressions
                            , getFrontendProcessorOutputDesugaredExpressions = desugared
                            , getFrontendProcessorOutputState = state
                            } <-
        traceStepWithDebugOutput (fileName ++ ".frontend") $
        processSourceFile moduleImports fileName tokens
    moduleExports <- handleResult $ processExports desugared state
    -- Select exported declarations and save
    writeToFile (fileName ++ ".exp") expressions
    writeToFile (fileName ++ ".out") moduleExports
    -- Combine state
    let moduleName = getModuleName header
    return $ defineModule moduleName moduleExports initialState
