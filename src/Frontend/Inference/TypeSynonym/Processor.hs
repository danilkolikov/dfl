{- |
Module      :  Frontend.Inference.TypeSynonym.Processor
Description :  Functions for expanding of type synonyms
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of expanding of type synonyms inference
-}
module Frontend.Inference.TypeSynonym.Processor
    ( processTypeSynonyms
    , TypeSynonymProcessorDebugOutput(..)
    , TypeSynonymProcessorError(..)
    , TypeSynonymExpandingError(..)
    ) where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Lazy as HM

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Signature
import Frontend.Inference.TypeSynonym.Base
import Frontend.Inference.TypeSynonym.Expander
import Util.Debug
import Frontend.Inference.Variables

type TypeSynonymProcessor
     = WithDebugOutput TypeSynonymProcessorError TypeSynonymProcessorDebugOutput

-- | A type of debug output of type synonym inference
data TypeSynonymProcessorDebugOutput = TypeSynonymProcessorDebugOutput
    { getTypeSynonymProcessorDebugOutputSignatures :: Maybe (Signatures TypeSignature)
    , getTypeSynonymProcessorDebugOutputAst :: Maybe AstWithKinds
    } deriving (Eq, Show)

instance Semigroup TypeSynonymProcessorDebugOutput where
    TypeSynonymProcessorDebugOutput s1 a1 <> TypeSynonymProcessorDebugOutput s2 a2 =
        TypeSynonymProcessorDebugOutput (s1 <|> s2) (a1 <|> a2)

instance Monoid TypeSynonymProcessorDebugOutput where
    mempty = TypeSynonymProcessorDebugOutput Nothing Nothing

-- | Expands type synonyms in the provided AST
processTypeSynonyms ::
       HM.HashMap Ident TypeConstructorSignature
    -> Signatures TypeSignature
    -> F.TypeSynonyms
    -> AstWithKinds
    -> ( Either TypeSynonymProcessorError ( Signatures TypeSignature
                                          , AstWithKinds)
       , TypeSynonymProcessorDebugOutput)
processTypeSynonyms typeConstructorSignatures initialSignatures typeSynonyms ast =
    runWithDebugOutput $
    processTypeSynonyms'
        typeConstructorSignatures
        initialSignatures
        typeSynonyms
        ast

processTypeSynonyms' ::
       HM.HashMap Ident TypeConstructorSignature
    -> Signatures TypeSignature
    -> F.TypeSynonyms
    -> AstWithKinds
    -> TypeSynonymProcessor (Signatures TypeSignature, AstWithKinds)
processTypeSynonyms' typeConstructorSignatures initialSignatures typeSynonyms ast = do
    newSignatures <-
        wrapEither id $
        processSignatures
            typeConstructorSignatures
            initialSignatures
            typeSynonyms
    writeDebugOutput
        mempty
            {getTypeSynonymProcessorDebugOutputSignatures = Just newSignatures}
    let allSignatures = initialSignatures <> newSignatures
    newAst <-
        wrapEither TypeSynonymProcessorErrorExpanding $
        expandModule allSignatures ast
    writeDebugOutput
        mempty {getTypeSynonymProcessorDebugOutputAst = Just newAst}
    return (newSignatures, newAst)
