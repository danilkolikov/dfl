{- |
Module      :  Compiler.Prettify.TypeSignatures
Description :  Prettifying of TypeSignatures
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of TypeSignatures
-}
module Compiler.Prettify.TypeSignatures where

import qualified Data.HashMap.Lazy as HM

import Compiler.Prettify.Utils
import Frontend.Inference.Processor

prettifyTypeSignatures :: TypeSignatures -> String
prettifyTypeSignatures TypeSignatures { getTypeSignaturesConstructors = constructors
                                      , getTypeSignaturesMethods = methods
                                      } =
    unlines
        [ prettifyHeader "Constructors:"
        , unlines . map prettifyTypeSignature . HM.toList $ constructors
        , prettifyHeader "Methods:"
        , unlines . map prettifyTypeSignature . HM.toList $ methods
        ]
