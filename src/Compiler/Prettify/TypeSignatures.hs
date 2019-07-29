{- |
Module      :  Compiler.Prettify.TypeSignatures
Description :  Prettifying of TypeSignatures
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of TypeSignatures
-}
module Compiler.Prettify.TypeSignatures where

import Data.Maybe (catMaybes)

import Compiler.Prettify.InferenceDebugOutput
import Compiler.Prettify.Utils
import Frontend.Inference.Processor

prettifyTypeSignatures :: TypeSignatures -> String
prettifyTypeSignatures TypeSignatures { getTypeSignaturesConstructors = constructors
                                      , getTypeSignaturesMethods = methods
                                      , getTypeSignaturesExpressions = expressions
                                      } =
    unlines
        [ prettifyHeader "Constructors:"
        , prettifySignatures constructors
        , prettifyHeader "Methods:"
        , prettifySignatures methods
        , prettifyHeader "Expressions:"
        , prettifySignatures expressions
        ]

prettifyTypeInferenceDebugOutput :: TypeInferenceDebugOutput -> String
prettifyTypeInferenceDebugOutput TypeInferenceDebugOutput { getTypeInferenceDebugOutputConstructorsOutput = constructors
                                                          , getTypeInferenceDebugOutputMethodsOutput = methods
                                                          , getTypeInferenceDebugOutputExpressions = expressions
                                                          } =
    let prettifyConstructors c =
            unlines
                [ prettifyHeader "Constructors:"
                , prettifySingleGroupDebugOutput c
                ]
        prettifyMethods m =
            unlines
                [prettifyHeader "Methods:", prettifySingleGroupDebugOutput m]
        prettifyExpressions e =
            unlines
                [prettifyHeader "Expressions:", prettifyInferenceDebugOutput e]
     in unlines . catMaybes $
        [ prettifyConstructors <$> constructors
        , prettifyMethods <$> methods
        , prettifyExpressions <$> expressions
        ]
