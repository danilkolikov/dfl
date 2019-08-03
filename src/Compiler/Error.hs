{- |
Module      :  Compiler.Error
Description :  Compilation errors
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Definitions of errors, which can be encountered during compilation
-}
module Compiler.Error where

import Frontend.Desugaring.Processor (DesugaringError)
import Frontend.Inference.Processor (CombinedInferenceError)
import Frontend.Syntax.Processor
    ( FixityResolutionError
    , LexicalError
    , ParserError
    )

-- | Errors which can be encountered during compilation
data CompilationError
    = CompilerErrorLexer LexicalError -- ^ Lexical error
    | CompilerErrorParser ParserError -- ^ Parser error
    | CompilerErrorFixity FixityResolutionError -- ^ Error of fixity resolution
    | CompilerErrorDesugaring DesugaringError -- ^ Desugaring error
    | CompilerErrorInference CombinedInferenceError -- ^ Error of type/kind inference
    deriving (Eq, Show)

-- | Class of types which can be converted to a CompilationError
class IsCompilationError a where
    wrapToCompilationError :: a -> CompilationError -- ^ Wrap object into CompilationError

instance IsCompilationError CompilationError where
    wrapToCompilationError = id

instance IsCompilationError LexicalError where
    wrapToCompilationError = CompilerErrorLexer

instance IsCompilationError ParserError where
    wrapToCompilationError = CompilerErrorParser

instance IsCompilationError FixityResolutionError where
    wrapToCompilationError = CompilerErrorFixity

instance IsCompilationError DesugaringError where
    wrapToCompilationError = CompilerErrorDesugaring

instance IsCompilationError CombinedInferenceError where
    wrapToCompilationError = CompilerErrorInference
