{- |
Module      :  Compiler.Processor
Description :  Processor of compilation steps
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of compilation of DFL files
-}
module Compiler.Processor where

import Data.Maybe (isJust)

import Compiler.Environment
import Compiler.Module.Processor
import Compiler.Monad

-- | Compiles source files
compile :: Environment -> IO Bool
compile environment = do
    result <- runCompiler compileMultipleModules environment
    return $ isJust result
