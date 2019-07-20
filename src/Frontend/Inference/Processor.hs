{- |
Module      :  Frontend.Inference.Processor
Description :  Processors of kind and type inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processors of kind and type inference
-}
module Frontend.Inference.Processor
    ( InferenceError(..)
    , Signatures(..)
    , TypeSynonymSignatures
    , Kind.KindInferenceDebugOutput(..)
    , Kind.KindInferenceGroupDebugOutput(..)
    , Kind.KindInferenceError(..)
    , ExpandTypeSynonymsOutput(..)
    , TypeSynonymsProcessingError(..)
    , Equalities(..)
    , DependencyGroupItem(..)
    , DependencyGroupItemEmpty
    , DependencyGroupItemWithSignature
    , emptySignatures
    , inferKinds
    , expandTypeSynonyms
    ) where

import Data.Bifunctor (bimap, first)

import Frontend.Desugaring.Final.Ast (Module(..))
import qualified Frontend.Inference.Kind.Processor as Kind
import Frontend.Inference.Kind.ProcessorBase
import Frontend.Inference.TypeSynonyms.Processor

-- | Errors which can be encounterd during inference
data InferenceError
    = InferenceErrorKind Kind.KindInferenceError -- ^ Kind inference error
    | InferenceErrorTypeSynonyms TypeSynonymsProcessingError
    deriving (Eq, Show)

-- | Infer kinds of types in a module
inferKinds ::
       Module
    -> Signatures
    -> (Either InferenceError Signatures, Kind.KindInferenceDebugOutput)
inferKinds module' state =
    first (first InferenceErrorKind) $
    Kind.inferKinds
        (getModuleDataTypes module')
        (getModuleTypeSynonyms module')
        (getModuleClasses module')
        state

-- | Output of a step that expands type synonyms
newtype ExpandTypeSynonymsOutput = ExpandTypeSynonymsOutput
    { getExpandTypeSynonymSignatures :: TypeSynonymSignatures -- ^ Get expanded signatures
    } deriving (Eq, Show)

-- | Expend defined type synonyms
expandTypeSynonyms ::
       Module -> Signatures -> Either InferenceError ExpandTypeSynonymsOutput
expandTypeSynonyms module' signatures =
    bimap InferenceErrorTypeSynonyms ExpandTypeSynonymsOutput $
    processSignatures
        (getModuleTypeSynonyms module')
        (getTypeSynonymSignatures signatures)
