{- |
Module      :  Compiler.Prettify.Output
Description :  Prettifying of Output
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of Output
-}
module Compiler.Prettify.Output where

import Compiler.Output
import Compiler.Prettify.FixityResolutionOutput (prettifyOperators)
import Compiler.Prettify.InferenceProcessorOutput ()
import Compiler.Prettify.Utils

prettifyOutput :: Output -> String
prettifyOutput Output { getInfixOperators = operators
                      , getInferenceOutput = inferenceOutput
                      } =
    unlines
        [ prettifyHeader "Infix operators:"
        , indentLines $ prettifyOperators operators
        , prettifyWithHeader "Inference output:" $ Indented inferenceOutput
        ]
