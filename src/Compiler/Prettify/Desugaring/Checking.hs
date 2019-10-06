{- |
Module      :  Compiler.Prettify.Desugaring.Checking
Description :  Prettifying of checking processor output
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of checking output
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.Desugaring.Checking where

import Compiler.Prettify.Ast ()
import Compiler.Prettify.Desugaring.Ast ()
import Compiler.Prettify.Desugaring.Grouping ()
import Compiler.Prettify.Utils
import Frontend.Desugaring.Checking.Processor

instance Prettifiable CheckingDebugOutput where
    prettify CheckingDebugOutput { getCheckingDebugOutputExports = exports
                                 , getCheckingDebugOutputTypeSynonyms = typeSynonyms
                                 , getCheckingDebugOutputDataTypes = dataTypes
                                 , getCheckingDebugOutputClassses = classes
                                 , getCheckingDebugOutputInstances = instances
                                 , getCheckingDebugOutputFunctions = functions
                                 } =
        unlineMaybes
            [ prettifyWithHeader "Exports:" <$> exports
            , prettifyWithHeader "Type synonyms:" <$> typeSynonyms
            , prettifyWithHeader "Data types:" <$> dataTypes
            , prettifyWithHeader "Classes:" <$> classes
            , prettifyWithHeader "Instances:" <$> instances
            , prettifyWithHeader "Functions:" <$> functions
            ]
