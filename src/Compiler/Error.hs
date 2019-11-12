{- |
Module      :  Compiler.Error
Description :  Compilation errors
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Definitions of errors, which can be encountered during compilation
-}
module Compiler.Error where

import Compiler.Module.Base
import Compiler.Prettify.CompilationError ()
import Compiler.Prettify.Utils
import Frontend.HeaderProcessor
import Frontend.Module.Export.Processor
import Frontend.Module.Import.Processor
import Frontend.Processor
import Util.DependencyResolver

-- | Class for types which represent a compilation error
class (Prettifiable a) =>
      IsCompilationError a


instance IsCompilationError FrontendProcessorError

instance IsCompilationError HeaderProcessorError

instance (Prettifiable a) =>
         IsCompilationError (DependencyResolverError a)

instance IsCompilationError DependencyBuilderError

instance IsCompilationError ImportProcessorError

instance IsCompilationError ExplicitProcessorError
