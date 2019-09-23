{- |
Module      :  Compiler.DebugOutput
Description :  Debug output of the compiler
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Debug output of the DFL compiler
-}
module Compiler.DebugOutput where

import Frontend.Desugaring.Processor (DesugaringOutput(..))
import Frontend.Inference.Processor (InferenceProcessorDebugOutput(..))
import Frontend.Syntax.Processor
    ( FixityResolutionOutput(..)
    , Module
    , TokenStream(..)
    )

import Compiler.Prettify.Ast ()
import Compiler.Prettify.DesugaringOutput (prettifyDesugaringOutput)
import Compiler.Prettify.FixityResolutionOutput (prettifyFixityResolutionOutput)
import Compiler.Prettify.InferenceProcessorDebugOutput ()
import Compiler.Prettify.PrettyPrintable (prettifyAst)
import Compiler.Prettify.TokenStream (prettifyTokenStream)
import Compiler.Prettify.Utils

-- | Debug output of a step
data DebugOutput = DebugOutput
    { getDebugOutputValue :: String -- ^ String with the debug output
    , getDebugOutputType :: DebugOutputType -- ^ Type of a debug output
    }

-- | Type of debug output
data DebugOutputType
    = DebugOutputTypeLexems -- ^ List of lexems
    | DebugOutputTypeAst -- ^ Abstract syntax tree
    | DebugOutputTypeFixityResolution -- ^ AST and fixity of operators
    | DebugOutputTypeDesugaredAst -- ^ Desugared AST
    | DebugOutputTypeInference -- ^ Output of inference

-- | Class for types which can be converted to the debug output
class HasDebugOutput a where
    getDebugOutput :: a -> DebugOutput -- ^ Convert object to the debug output

instance HasDebugOutput TokenStream where
    getDebugOutput a = DebugOutput (prettifyTokenStream a) DebugOutputTypeLexems

instance HasDebugOutput Module where
    getDebugOutput a = DebugOutput (prettifyAst a) DebugOutputTypeAst

instance HasDebugOutput FixityResolutionOutput where
    getDebugOutput a =
        DebugOutput
            (prettifyFixityResolutionOutput a)
            DebugOutputTypeFixityResolution

instance HasDebugOutput DesugaringOutput where
    getDebugOutput a =
        DebugOutput (prettifyDesugaringOutput a) DebugOutputTypeDesugaredAst

instance HasDebugOutput InferenceProcessorDebugOutput where
    getDebugOutput a = DebugOutput (prettify a) DebugOutputTypeInference
