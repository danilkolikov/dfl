{- |
Module      :  Compiler.Processor
Description :  Processor of compilation steps
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of compilation of DFL files
-}
module Compiler.Processor where

import qualified Data.HashMap.Lazy as HM

import Compiler.Base
import Compiler.Output
import Frontend.Desugaring.Processor
import Frontend.Inference.Processor
import Frontend.Syntax.Processor

-- | Compile a single source file
compileSourceFile :: (Compiler m) => m ()
compileSourceFile = do
    let initialInfixOperators = HM.empty
        initialDesugaringState = emptyDesugaringState
        initialKindInferenceState = emptySignatures
    fileName <- getSourceFileName
    fileContent <- readFileContent fileName
    lexems <- traceStep $ lexicalAnalysis fileName fileContent
    ast <- traceStep $ parsing fileName lexems
    FixityResolutionOutput { getFixityResolutionOutputAst = resolvedFixity
                           , getFixityResolutionOutputOperators = infixOperators
                           } <-
        traceStep $ fixityResolution initialInfixOperators ast
    DesugaringOutput {getDesugaringOutputAst = desugared} <-
        traceStep $ desugarParsedModule resolvedFixity initialDesugaringState
    inferredKinds <-
        traceStepWithDebugOutput $
        inferKinds desugared initialKindInferenceState
    writeOutput
        Output
            { getInfixOperators = infixOperators
            , getInferredKinds = inferredKinds
            }
