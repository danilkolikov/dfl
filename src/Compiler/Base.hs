{- |
Module      :  Compiler.Base
Description :  Base definitions for the DFL compiler
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Definitions of base functions for compilation of files
-}
module Compiler.Base where

import Control.Monad (when)

import Compiler.DebugOutput
import Compiler.Environment
import Compiler.Error
import Frontend.Processor

-- | A class of objects which can compile DFL source files
class (Monad m) =>
      Compiler m
    where
    getEnvironmentComponent :: (Environment -> a) -> m a -- ^ Get a component of the environment
    readFileContent :: String -> m String -- ^ Read content of a source file
    handleResult :: (IsCompilationError e) => Either e a -> m a -- ^ Handle result of a step of compilation
    writeDebugOutput :: (IsDebugOutput d) => d -> m () -- ^ Write debug output of a step
    writeOutput :: FrontendProcessorOutput -> m () -- ^ Write result of compilation

-- | Get the name of a source file to compile
getSourceFileName :: (Compiler m) => m String
getSourceFileName = getEnvironmentComponent getSourceFile

-- | Writes a debug output and executes a single step
traceStepWithDebugOutput ::
       (Compiler m, IsCompilationError e, IsDebugOutput d)
    => (Either e a, d)
    -> m a
traceStepWithDebugOutput (step, debugOutput) = do
    shouldTrace <- getEnvironmentComponent isDebugOutputEnabled
    when shouldTrace . writeDebugOutput $ debugOutput
    handleResult step
