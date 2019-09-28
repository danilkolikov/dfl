{- |
Module      :  Compiler.Processor
Description :  Processor of compilation steps
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of compilation of DFL files
-}
module Compiler.Processor where

import Compiler.Base
import Compiler.Prettify.CompilationError ()
import Frontend.Processor

-- | Compile a single source file
compileSourceFile :: (Compiler m) => m ()
compileSourceFile = do
    let initialState = emptyFrontendProcessorOutput
    fileName <- getSourceFileName
    fileContent <- readFileContent fileName
    HeaderProcessorOutput {getHeaderProcessorOutputTokens = tokens} <-
        traceStepWithDebugOutput $ processModuleHeader fileName fileContent
    output <-
        traceStepWithDebugOutput $
        processSourceFile initialState fileName tokens
    writeOutput output
