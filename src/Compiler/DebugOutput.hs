{- |
Module      :  Compiler.DebugOutput
Description :  Debug output of the compiler
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Debug output of the DFL compiler
-}
module Compiler.DebugOutput where

import Compiler.Prettify.FrontendProcessorOutput ()
import Compiler.Prettify.SyntaxProcessorDebugOutput ()
import Compiler.Prettify.Utils
import Frontend.Processor

-- | Type of debug output
data DebugOutputType
    = DebugOutputTypeHeader -- ^ Header parser debug output
    | DebugOutputTypeFrontend -- ^ Frontend debug output

-- | Class for types which can be converted to the debug output
class (Prettifiable a) =>
      IsDebugOutput a
    where
    getDebugOutputType :: a -> DebugOutputType -- ^ Get type of the debug output

instance IsDebugOutput FrontendProcessorDebugOutput where
    getDebugOutputType _ = DebugOutputTypeFrontend

instance IsDebugOutput HeaderProcessorDebugOutput where
    getDebugOutputType _ = DebugOutputTypeHeader
