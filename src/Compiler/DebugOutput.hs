{- |
Module      :  Compiler.DebugOutput
Description :  Debug output of the compiler
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Debug output of the DFL compiler
-}
module Compiler.DebugOutput where

import Frontend.Syntax.Processor (TokenStream(..), Module, FixityResolutionOutput(..))
import Frontend.Desugaring.Processor (DesugaringOutput(..))
import Frontend.Inference.Processor (KindInferenceState(..))

-- | Debug output of a step
data DebugOutput = DebugOutput
    { getDebugOutputValue :: String -- ^ String with the debug output
    , getDebugOutputType :: DebugOutputType -- ^ Type of a debug output
    }

-- | Type of debug output
data DebugOutputType
    = DebugOutputTypeLexems -- ^ List of lexems
    | DebugOutputTypeAst -- ^ Abstract syntax tree
    | DebugOutputTypeFixityResolution  -- ^ AST and fixity of operators
    | DebugOutputTypeDesugaredAst -- ^ Desugared AST
    | DebugOutputTypeInferredKinds -- ^ Inferred kind information

-- | Class for types which can be converted to the debug output
class HasDebugOutput a where
    getDebugOutput :: a -> DebugOutput -- ^ Convert object to the debug output

instance HasDebugOutput TokenStream where
  getDebugOutput a = DebugOutput (show a) DebugOutputTypeLexems

instance HasDebugOutput Module where
  getDebugOutput a = DebugOutput (show a) DebugOutputTypeAst

instance HasDebugOutput FixityResolutionOutput where
  getDebugOutput a = DebugOutput (show a) DebugOutputTypeFixityResolution

instance HasDebugOutput DesugaringOutput where
  getDebugOutput a = DebugOutput (show a) DebugOutputTypeDesugaredAst

instance HasDebugOutput KindInferenceState where
  getDebugOutput a = DebugOutput (show a) DebugOutputTypeInferredKinds
