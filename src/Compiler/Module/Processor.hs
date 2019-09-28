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
import Compiler.Module.Export
import Compiler.Module.Import
import Frontend.Processor
import Util.DependencyResolver

-- | Finds dependencies between modules and compiles them
compileMultipleModules :: (Compiler m) => m ()
compileMultipleModules = do
    dependencies <- resolveDependencies
    (_, compilation) <-
        handleResult $
        traverseGraph compileModule emptyModuleProcessorState dependencies
    _ <- compilation
    return ()

compileModule ::
       (Compiler m)
    => ModuleProcessorState
    -> HS.HashSet FileName
    -> m ModuleProcessorState
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
    let importedState = selectImports initialState header
    output <-
        traceStepWithDebugOutput (fileName ++ ".frontend") $
        processSourceFile importedState fileName tokens
    -- Select exported declarations and save
    let exportedState = selectExports output header
    writeToFile (fileName ++ ".out") exportedState
    -- Combine state
    return $ saveModule fileName exportedState initialState
