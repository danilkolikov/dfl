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
import Compiler.Prettify.Utils

prettifyOutput :: Output -> String
prettifyOutput Output { getInfixOperators = operators
                      , getInferredKinds = kinds
                      , getExpandedTypeSynonyms = signatures
                      } =
    unlines
        [ prettifyHeader "Infix operators"
        , prettifyOperators operators
        , prettifyHeader "Inferred kinds"
        , prettify kinds
        , prettifyHeader "Type synonyms"
        , prettify signatures
        ]
