{- |
Module      :  Compiler.DebugOutput
Description :  Debug output of the compiler
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Debug output of the DFL compiler
-}
module Compiler.DebugOutput where

import Compiler.Module.Base
import Compiler.Prettify.DependencyBuilderDebugOutput ()
import Compiler.Prettify.FrontendProcessorOutput ()
import Compiler.Prettify.SyntaxProcessorDebugOutput ()
import Compiler.Prettify.Utils
import Frontend.HeaderProcessor
import Frontend.Processor

-- | Class for types which can be converted to the debug output
class (Prettifiable a) =>
      IsDebugOutput a


instance IsDebugOutput FrontendProcessorDebugOutput

instance IsDebugOutput HeaderProcessorDebugOutput

instance IsDebugOutput DependencyBuilderDebugOutput
