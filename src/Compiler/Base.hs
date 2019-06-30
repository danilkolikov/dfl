{- |
Module      :  Compiler.Base
Description :  Base definitions for the DFL compiler
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Definitions of base functions for compilation of files
-}
module Compiler.Base where

import Control.Monad (when)
import Data.Bifunctor (first)

import Compiler.DebugOutput
import Compiler.Environment
import Compiler.Error
import Compiler.Output

-- | Class of objects which can compile DFL source files
class (Monad m) =>
      Compiler m
    where
    getEnvironmentComponent :: (Environment -> a) -> m a -- ^ Get a component of the environment
    readFileContent :: String -> m String -- ^ Read content of a source file
    handleResult :: Either CompilationError a -> m a -- ^ Handle result of a step of compilation
    writeDebugOutput :: DebugOutput -> m () -- ^ Write debug output of a step
    writeOutput :: Output -> m () -- ^ Write result of compilation

-- | Get name of a source file to compile
getSourceFileName :: (Compiler m) => m String
getSourceFileName = getEnvironmentComponent getSourceFile

-- | Execute single step of compilation
processStep :: (Compiler m, IsCompilationError e) => Either e a -> m a
processStep = handleResult . first wrapToCompilationError

-- | Execute single step and write debug output
traceStep ::
       (Compiler m, IsCompilationError e, HasDebugOutput a) => Either e a -> m a
traceStep step = do
    result <- processStep step
    shouldTrace <- getEnvironmentComponent isDebugOutputEnabled
    when shouldTrace . writeDebugOutput . getDebugOutput $ result
    return result
