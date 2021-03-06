{- |
Module      :  Compiler.Prettify.FrontendProcessorOutput
Description :  Prettifying of Output
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of Output
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.FrontendProcessorOutput where

import Compiler.Prettify.Desugaring.Processor ()
import Compiler.Prettify.InferenceProcessorDebugOutput ()
import Compiler.Prettify.InferenceProcessorOutput ()
import Compiler.Prettify.SyntaxProcessorDebugOutput ()
import Compiler.Prettify.Utils
import Frontend.Processor

instance Prettifiable FrontendProcessorOutput where
    prettify FrontendProcessorOutput {getFrontendProcessorOutputExpressions = expressions} =
        unlines' [prettifyWithHeader "Translated expressions:" expressions]

instance Prettifiable FrontendProcessorDebugOutput where
    prettify FrontendProcessorDebugOutput { getFrontendProcessorDebugOutputSyntax = syntax
                                          , getFrontendProcessorDebugOutputDesugaring = desugaring
                                          , getFrontendProcessorDebugOutputInference = inference
                                          } =
        unlineMaybes
            [ prettifyWithHeader "Syntax output:" <$> syntax
            , prettifyWithHeader "Desugaring output:" <$> desugaring
            , prettifyWithHeader "Inference output:" <$> inference
            ]
