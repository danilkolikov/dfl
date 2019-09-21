{- |
Module      :  Compiler.DebugOutput
Description :  Debug output of the compiler
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Debug output of the DFL compiler
-}
module Compiler.DebugOutput where

import Frontend.Desugaring.Processor (DesugaringOutput(..))
import Frontend.Inference.Processor
    ( ExpandTypeSynonymsOutput
    , InferenceDebugOutput(..)
    )
import Frontend.Syntax.Processor
    ( FixityResolutionOutput(..)
    , Module
    , TokenStream(..)
    )

import Compiler.Prettify.Ast (prettifyAst)
import Compiler.Prettify.DesugaringOutput (prettifyDesugaringOutput)
import Compiler.Prettify.ExpandTypeSynonymsOutput
    ( prettifyExpandTypeSynonymsOutput
    )
import Compiler.Prettify.FixityResolutionOutput (prettifyFixityResolutionOutput)
import Compiler.Prettify.InferenceDebugOutput (prettifyInferenceDebugOutput)
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
    | DebugOutputTypeInferredKinds -- ^ Inferred kind information
    | DebugOutputTypeTypeSynonyms -- ^ Expanded type synonyms
    | DebugOutputTypeInferredTypes -- ^ Inferred types

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

instance (Prettifiable a, Prettifiable s) =>
         HasDebugOutput (InferenceDebugOutput a s) where
    getDebugOutput a =
        DebugOutput
            (prettifyInferenceDebugOutput a)
            DebugOutputTypeInferredKinds

instance HasDebugOutput ExpandTypeSynonymsOutput where
    getDebugOutput a =
        DebugOutput
            (prettifyExpandTypeSynonymsOutput a)
            DebugOutputTypeTypeSynonyms
