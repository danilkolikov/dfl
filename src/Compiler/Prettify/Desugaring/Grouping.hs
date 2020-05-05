{- |
Module      :  Compiler.Prettify.Desugaring.Grouping
Description :  Prettifying of Grouping processor output
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of Grouping output
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.Desugaring.Grouping where

import Compiler.Prettify.Ast ()
import Compiler.Prettify.Desugaring.Ast ()
import Compiler.Prettify.PrettyPrintable
import Compiler.Prettify.PrettyPrinter
import Compiler.Prettify.Utils
import Frontend.Desugaring.Grouping.Ast
import Frontend.Desugaring.Grouping.Processor

instance Prettifiable GroupingProcessorState where
    prettify GroupingProcessorState { getGroupingProcessorStateTypes = types
                                    , getGroupingProcessorStateExpressions = expressions
                                    } =
        unlines'
            [ prettifyWithHeader "Types:" types
            , prettifyWithHeader "Expressions:" expressions
            ]

instance Prettifiable GroupingDebugOutput where
    prettify GroupingDebugOutput { getGroupingDebugOutputState = state
                                 , getGroupingDebugOutputTypeSynonyms = typeSynonyms
                                 , getGroupingDebugOutputDataTypes = dataTypes
                                 , getGroupingDebugOutputClassses = classes
                                 , getGroupingDebugOutputInstances = instances
                                 , getGroupingDebugOutputFunctions = functions
                                 } =
        unlineMaybes
            [ prettifyWithHeader "State:" <$> state
            , prettifyWithHeader "Type synonyms:" <$> typeSynonyms
            , prettifyWithHeader "Data types:" <$> dataTypes
            , prettifyWithHeader "Classes:" <$> classes
            , prettifyWithHeader "Instances:" <$> instances
            , prettifyWithHeader "Functions:" <$> functions
            ]

instance PrettyPrintable Exp where
    prettyPrint = singleLine . show
