{- |
Module      :  Compiler.Prettify.InferenceProcessorDebugOutput
Description :  Prettifying of InferenceProcessorDebugOutput
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of InferenceProcessorDebugOutput
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.InferenceProcessorDebugOutput where

import Data.Maybe (catMaybes)

import Compiler.Prettify.AstWithKinds ()
import Compiler.Prettify.DesugaredAst ()
import Compiler.Prettify.Expression ()
import Compiler.Prettify.InferenceDebugOutput
import Compiler.Prettify.LetExpression ()
import Compiler.Prettify.Utils
import Frontend.Inference.Class (DefaultInstance(..))
import Frontend.Inference.Class.Base
import Frontend.Inference.Processor

instance Prettifiable InferenceProcessorDebugOutput where
    prettify InferenceProcessorDebugOutput { getInferenceProcessorDebugOutputKinds = kinds
                                           , getInferenceProcessorDebugOutputTypeSynonyms = typeSynonyms
                                           , getInferenceProcessorDebugOutputClasses = classes
                                           , getInferenceProcessorDebugOutputDataTypes = dataTypes
                                           , getInferenceProcessorDebugOutputInstances = instances
                                           , getInferenceProcessorDebugOutputLet = let'
                                           , getInferenceProcessorDebugOutputType = type'
                                           , getInferenceProcessorDebugOutputTranslation = translation
                                           } =
        unlines' $
        catMaybes
            [ prettifyWithHeader "Kinds inference:" . Indented <$> kinds
            , prettifyWithHeader "Type Synonyms:" . Indented <$> typeSynonyms
            , prettifyWithHeader "Classes:" . Indented <$> classes
            , prettifyWithHeader "Data types:" . Indented <$> dataTypes
            , prettifyWithHeader "Instances:" . Indented <$> instances
            , prettifyWithHeader "Let desugaring:" . Indented <$> let'
            , prettifyWithHeader "Type inference:" . Indented <$> type'
            , prettifyWithHeader "Polymorphic translation:" . Indented <$>
              translation
            ]

instance Prettifiable KindProcessorDebugOutput where
    prettify KindProcessorDebugOutput { getKindProcessorDebugOutputInference = inference
                                      , getKindProcessorDebugOutputInstances = instances
                                      , getKindProcessorDebugOutputCheck = check
                                      } =
        unlines' $
        catMaybes
            [ prettifyWithHeader "Inference:" . Indented <$> inference
            , prettifyWithHeader "Instance check:" . Indented <$> instances
            , prettifyWithHeader "Ast Check:" . Indented <$> check
            ]

instance Prettifiable TypeSynonymProcessorDebugOutput where
    prettify TypeSynonymProcessorDebugOutput { getTypeSynonymProcessorDebugOutputSignatures = signatures
                                             , getTypeSynonymProcessorDebugOutputAst = astWithKinds
                                             } =
        unlines' $
        catMaybes
            [ prettifyWithHeader "Type synonym signatures:" . Indented <$>
              signatures
            , prettifyWithHeader "AST:" . Indented <$> astWithKinds
            ]

instance Prettifiable ClassProcessorDebugOutput where
    prettify ClassProcessorDebugOutput { getClassProcessorDebugOutputDependencyGraph = graph
                                       , getClassProcessorDebugOutputDependencyGroups = groups
                                       , getClassProcessorDebugOutputOutputs = outputs
                                       } =
        unlines' $
        catMaybes
            [ prettifyGraph <$> graph
            , prettifyGroups <$> groups
            , prettifyWithHeader "Group outputs:" . Indented <$> outputs
            ]

instance Prettifiable ClassProcessorOutput where
    prettify ClassProcessorOutput { getClassProcessorOutputClasses = classes
                                  , getClassProcessorOutputDataTypes = dataTypes
                                  , getClassProcessorOutputSignatures = typeSignatures
                                  , getClassProcessorOutputDefaultInstances = defaultInstances
                                  , getClassProcessorOutputMethods = methods
                                  , getClassProcessorOutputGetters = getters
                                  } =
        unlines'
            [ prettifyWithHeader "Classes:" $ Indented classes
            , prettifyWithHeader "Data types:" $ Indented dataTypes
            , prettifyWithHeader "Signatures:" $ Indented typeSignatures
            , prettifyWithHeader "Default instances:" $
              Indented defaultInstances
            , prettifyWithHeader "Methods:" $ Indented methods
            , prettifyWithHeader "Getters:" $ Indented getters
            ]

instance Prettifiable DefaultInstance where
    prettify DefaultInstance { getDefaultInstanceClassName = className
                             , getDefaultInstanceParam = param
                             , getDefaultInstanceMethods = methods
                             } =
        unlines'
            [ unwords ["Default instance: ", prettify className, prettify param]
            , indentLines $ unlines' ["Methods:", prettify $ Indented methods]
            ]

instance Prettifiable DataTypeProcessorDebugOutput where
    prettify DataTypeProcessorDebugOutput { getDataTypeProcessorDebugOutputConstructors = constructors
                                          , getDataTypeProcessorDebugOutputInstances = instances
                                          } =
        unlines' $
        catMaybes
            [ prettifyWithHeader "Constructors:" . Indented <$> constructors
            , prettifyWithHeader "Generated instances:" . Indented <$> instances
            ]

instance Prettifiable InstanceProcessorDebugOutput where
    prettify InstanceProcessorDebugOutput { getInstanceProcessorDebugOutputInstances = instances
                                          , getInstanceProcessorDebugOutputDefaults = defaultExps
                                          , getInstanceProcessorDebugOutputExpressions = expressions
                                          } =
        unlines' $
        catMaybes
            [ prettifyWithHeader "Instances:" . Indented <$> instances
            , prettifyWithHeader "Default expressions:" . Indented <$>
              defaultExps
            , prettifyWithHeader "Expressions:" . Indented <$> expressions
            ]

instance Prettifiable TranslationProcessorDebugOutput where
    prettify TranslationProcessorDebugOutput {getTranslationProcessorDebugOutputExpressions = expressions} =
        unlines' $
        catMaybes [prettifyWithHeader "Expressions" . Indented <$> expressions]
