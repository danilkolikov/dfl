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
import Compiler.Prettify.Utils

-- | A class of objects which can compile DFL source files
class (Monad m) =>
      Compiler m
    where
    getEnvironmentComponent :: (Environment -> a) -> m a -- ^ Get a component of the environment
    readFileContent :: String -> m String -- ^ Read content of a source file
    doesFileExist :: String -> m Bool -- ^ Check if file exist
    handleResult :: (IsCompilationError e) => Either e a -> m a -- ^ Handle result of a step of compilation
    writeToFile :: (Prettifiable a) => String -> a -> m () -- ^ Write output to file

-- | Writes a debug output and executes a single step
traceStepWithDebugOutput ::
       (Compiler m, IsCompilationError e, IsDebugOutput d)
    => String
    -> (Either e a, d)
    -> m a
traceStepWithDebugOutput outputFile (step, debugOutput) = do
    shouldTrace <- getEnvironmentComponent isDebugOutputEnabled
    when shouldTrace . writeToFile outputFile $ debugOutput
    handleResult step

-- | Raises a compilation error
raiseCompilationError :: (Compiler m, IsCompilationError e) => e -> m a
raiseCompilationError = handleResult . Left
