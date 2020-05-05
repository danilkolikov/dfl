{- |
Module      :  Frontend.Processor
Description :  Processor of frontend steps
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of DFL frontend
-}
module Frontend.Processor
    ( FrontendProcessorError(..)
    , FrontendState(..)
    , FrontendProcessorOutput(..)
    , emptyFrontendProcessorOutput
    , FrontendProcessorDebugOutput(..)
    , processSourceFile
    ) where

import Frontend.Base
import Frontend.Desugaring.Processor
import Frontend.Inference.Processor
import Frontend.Syntax.Processor
import Util.Debug

-- | Processes a source file
processSourceFile ::
       FrontendState
    -> String
    -> TokenStream
    -> ( Either FrontendProcessorError FrontendProcessorOutput
       , FrontendProcessorDebugOutput)
processSourceFile state fileName stream =
    runWithDebugOutput $ do
        let FrontendState { getFrontendStateDesugaring = groups
                          , getFrontendStateFixity = fixity
                          , getFrontendStateInference = inference
                          } = state
        ast <-
            wrapErrorAndDebugOutput
                FrontendProcessorErrorSyntax
                (\debug ->
                     mempty {getFrontendProcessorDebugOutputSyntax = Just debug}) $
            processModuleSyntax fileName stream
        DesugaringOutput { getDesugaringOutputModule = desugaredAst
                         , getDesugaringOutputGroups = desugaringGroups
                         , getDesugaringOutputFixity = desugaringFixity
                         } <-
            wrapErrorAndDebugOutput
                FrontendProcessorErrorDesugaring
                (\debug ->
                     mempty
                         { getFrontendProcessorDebugOutputDesugaring =
                               Just debug
                         }) $
            desugarParsedModule groups fixity ast
        inferenceOutput <-
            wrapErrorAndDebugOutput
                FrontendProcessorErrorInference
                (\debug ->
                     mempty
                         {getFrontendProcessorDebugOutputInference = Just debug}) $
            processModule inference desugaredAst
        let processorState =
                FrontendState
                    { getFrontendStateDesugaring = desugaringGroups
                    , getFrontendStateFixity = desugaringFixity
                    , getFrontendStateInference = inferenceOutput
                    }
            translated = getInferenceProcessorOutputExpressions inferenceOutput
        return
            FrontendProcessorOutput
                { getFrontendProcessorOutputState = processorState
                , getFrontendProcessorOutputDesugaredExpressions = desugaredAst
                , getFrontendProcessorOutputExpressions = translated
                }
