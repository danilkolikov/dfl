{- |
Module      :  Compiler.Error
Description :  Compilation errors
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Definitions of errors, which can be encountered during compilation
-}
module Compiler.Error where

import Compiler.Prettify.CompilationError ()
import Compiler.Prettify.Utils
import Frontend.Processor

-- | Class for types which represent a compilation error
class (Prettifiable a) =>
      IsCompilationError a


instance IsCompilationError FrontendProcessorError

instance IsCompilationError HeaderProcessorError
