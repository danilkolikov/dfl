{- |
Module      :  Compiler.Prettify.Output
Description :  Prettifying of Output
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of Output
-}
module Compiler.Prettify.Output where

import Compiler.Output
import qualified Compiler.Prettify.ExpandTypeSynonymsOutput as TypeSynonyms
    ( prettifySignatures
    )
import Compiler.Prettify.FixityResolutionOutput (prettifyOperators)
import qualified Compiler.Prettify.KindInferenceDebugOutput as Kind
    ( prettifySignatures
    )
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
        , Kind.prettifySignatures kinds
        , prettifyHeader "Type synonyms"
        , TypeSynonyms.prettifySignatures signatures
        ]
