{- |
Module      :  Compiler.Prettify.InferenceProcessorOutput
Description :  Prettifying of InferenceProcessorOutput
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of InferenceProcessorOutput
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.InferenceProcessorOutput where

import Compiler.Prettify.Expression ()
import Compiler.Prettify.Utils
import Frontend.Inference.Processor

instance Prettifiable InferenceProcessorOutput where
    prettify InferenceProcessorOutput { getInferenceProcessorOutputTypeConstructors = typeSignatures
                                      , getInferenceProcessorOutputTypeSynonyms = typeSynonyms
                                      , getInferenceProcessorOutputClasses = classes
                                      , getInferenceProcessorOutputInstances = instances
                                      , getInferenceProcessorOutputConstructors = constructors
                                      , getInferenceProcessorOutputMethods = methods
                                      , getInferenceProcessorOutputExpressions = expressions
                                      } =
        unlines'
            [ prettifyWithHeader "Type Constructor Signatures:" $
              Indented typeSignatures
            , prettifyWithHeader "Type Synonyms:" $ Indented typeSynonyms
            , prettifyWithHeader "Classes:" $ Indented classes
            , prettifyWithHeader "Instances:" $ Indented instances
            , prettifyWithHeader "Constructors:" $ Indented constructors
            , prettifyWithHeader "Methods:" $ Indented methods
            , prettifyWithHeader "Expressions:" $ Indented expressions
            ]
