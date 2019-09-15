{- |
Module      :  Frontend.Inference.TypeSynonyms.Processor
Description :  Functions for expanding of type synonyms
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of expanding of type synonyms inference
-}
module Frontend.Inference.TypeSynonyms.Processor
    ( processTypeSynonyms
    , TypeSynonymsDebugOutput(..)
    , TypeSynonymsProcessingError(..)
    , TypeSynonymsExpandingError(..)
    ) where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Lazy as HM

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Signature
import Frontend.Inference.TypeSynonyms.Base
import Frontend.Inference.TypeSynonyms.Expander
import Frontend.Inference.Util.Debug
import Frontend.Inference.Variables

type TypeSynonymsProcessor
     = WithDebugOutput TypeSynonymsProcessingError TypeSynonymsDebugOutput

-- | A type of debug output of type synonym inference
data TypeSynonymsDebugOutput = TypeSynonymsDebugOutput
    { getTypeSynonymsDebugOutputSignatures :: Maybe (Signatures TypeSignature)
    , getTypeSynonymsDebugOutputAst :: Maybe AstWithKinds
    }

instance Semigroup TypeSynonymsDebugOutput where
    TypeSynonymsDebugOutput s1 a1 <> TypeSynonymsDebugOutput s2 a2 =
        TypeSynonymsDebugOutput (s1 <|> s2) (a1 <|> a2)

instance Monoid TypeSynonymsDebugOutput where
    mempty = TypeSynonymsDebugOutput Nothing Nothing

-- | Expands type synonyms in the provided AST
processTypeSynonyms ::
       HM.HashMap Ident TypeConstructorSignature
    -> Signatures TypeSignature
    -> F.TypeSynonyms
    -> AstWithKinds
    -> ( Either TypeSynonymsProcessingError ( Signatures TypeSignature
                                            , AstWithKinds)
       , TypeSynonymsDebugOutput)
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
    -> TypeSynonymsProcessor (Signatures TypeSignature, AstWithKinds)
processTypeSynonyms' typeConstructorSignatures initialSignatures typeSynonyms ast = do
    newSignatures <-
        wrapEither id $
        processSignatures
            typeConstructorSignatures
            initialSignatures
            typeSynonyms
    writeDebugOutput
        mempty {getTypeSynonymsDebugOutputSignatures = Just newSignatures}
    let allSignatures = initialSignatures <> newSignatures
    newAst <-
        wrapEither TypeSynonymsProcessingErrorExpanding $
        expandModule allSignatures ast
    writeDebugOutput mempty {getTypeSynonymsDebugOutputAst = Just newAst}
    return (newSignatures, newAst)
