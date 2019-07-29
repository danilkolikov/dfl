{- |
Module      :  Compiler.Prettify.ExpandTypeSynonymsOutput
Description :  Prettifying of ExpandTypeSynonymsOutput
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of ExpandTypeSynonymsOutput
-}
module Compiler.Prettify.ExpandTypeSynonymsOutput where

import Compiler.Prettify.Utils
import Frontend.Inference.Processor

prettifyExpandTypeSynonymsOutput :: ExpandTypeSynonymsOutput -> String
prettifyExpandTypeSynonymsOutput (ExpandTypeSynonymsOutput synonyms) =
    unlines [prettifyHeader "Type Synonyms", prettifySignatures synonyms]
