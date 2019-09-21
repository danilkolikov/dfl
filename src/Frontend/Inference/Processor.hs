{- |
Module      :  Frontend.Inference.Processor
Description :  Processors of kind and type inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processors of kind and type inference
-}
module Frontend.Inference.Processor
    ( CombinedInferenceError(..)
    , I.InferenceError(..)
    , I.InferenceDebugOutput(..)
    , ExpandTypeSynonymsOutput(..)
    , TypeSynonymsProcessingError(..)
    , Signatures
    , TypeConstructorSignature
    , TypeSignature
    , Kind.KindInferenceEnvironmentItem(..)
    , defaultTypeSynonyms
    , defaultKindSignatures
    , inferKinds
    , expandTypeSynonyms
    ) where

import Data.Bifunctor (bimap, first)

import Frontend.Desugaring.Final.Ast (Module(..))
import Frontend.Inference.BuiltIns
import qualified Frontend.Inference.InferenceProcessor as I
import qualified Frontend.Inference.Kind.Base as Kind
import Frontend.Inference.Signature
import Frontend.Inference.TypeSynonyms.Processor

-- | Errors which can be encounterd during inference
data CombinedInferenceError
    = CombinedInferenceErrorInference I.InferenceError -- ^ Inference error
    | CombinedInferenceErrorTypeSynonyms TypeSynonymsProcessingError -- ^ An error of type synonyms expanding
    deriving (Eq, Show)

-- | Infer kinds of types in a module
inferKinds ::
       Module
    -> Signatures TypeConstructorSignature
    -> ( Either CombinedInferenceError (Signatures TypeConstructorSignature)
       , I.InferenceDebugOutput Kind.KindInferenceEnvironmentItem TypeConstructorSignature)
inferKinds module' state =
    first (first CombinedInferenceErrorInference) $
    Kind.inferKindsOfModule state module'

-- | Output of a step that expands type synonyms
newtype ExpandTypeSynonymsOutput = ExpandTypeSynonymsOutput
    { getExpandTypeSynonymSignatures :: Signatures TypeSignature -- ^ Get expanded signatures
    } deriving (Eq, Show)

-- | Expend defined type synonyms
expandTypeSynonyms ::
       Module
    -> Signatures TypeConstructorSignature
    -> Signatures TypeSignature
    -> Either CombinedInferenceError ExpandTypeSynonymsOutput
expandTypeSynonyms _ _ _ =
    bimap CombinedInferenceErrorTypeSynonyms ExpandTypeSynonymsOutput $
    error "TODO: rewrite this method"
