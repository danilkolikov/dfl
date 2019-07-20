{- |
Module      :  Compiler.Prettify.ExpandTypeSynonymsOutput
Description :  Prettifying of ExpandTypeSynonymsOutput
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of ExpandTypeSynonymsOutput
-}
module Compiler.Prettify.ExpandTypeSynonymsOutput where

import qualified Data.HashMap.Lazy as HM

import Compiler.Prettify.Utils
import Frontend.Desugaring.Final.Ast hiding (TypeSignature(..))
import Frontend.Inference.Processor
import Frontend.Inference.Signature

prettifyExpandTypeSynonymsOutput :: ExpandTypeSynonymsOutput -> String
prettifyExpandTypeSynonymsOutput (ExpandTypeSynonymsOutput synonyms) =
    unlines [prettifyHeader "Type Synonyms", prettifySignatures synonyms]

prettifySignatures :: TypeSynonymSignatures -> String
prettifySignatures = unlines . map prettifySignature . HM.toList

prettifySignature :: (Ident, TypeSignature) -> String
prettifySignature (name, sig@TypeSignature { getTypeSignatureKindParams = kindParams
                                           , getTypeSignatureTypeParams = typeParams
                                           , getTypeSignatureType = type'
                                           }) =
    unwords
        [ prettifyIdent name
        , "::"
        , prettifyForAll typeParams ++ prettifyType type'
        , "::"
        , prettifyForAll kindParams ++ prettifyKind (getFullKind sig)
        , "::"
        , prettifySort (getFullSort sig)
        ]
