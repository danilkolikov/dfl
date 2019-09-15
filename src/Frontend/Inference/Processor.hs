{- |
Module      :  Frontend.Inference.Processor
Description :  Processors of kind and type inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processors of kind and type inference
-}
module Frontend.Inference.Processor
    ( CombinedInferenceError(..)
    , InferenceError(..)
    , InferenceDebugOutput(..)
    , Type.TypeInferenceDebugOutput(..)
    , Type.TypeSignatures(..)
    , TypeSynonymSignatures
    , ExpandTypeSynonymsOutput(..)
    , TypeSynonymsProcessingError(..)
    , Signatures
    , TypeConstructorSignature
    , TypeSignature
    , Kind.KindInferenceEnvironmentItem(..)
    , defaultTypeSignatures
    , defaultTypeSynonyms
    , defaultKindSignatures
    , inferKinds
    , expandTypeSynonyms
    , inferTypes
    ) where

import Data.Bifunctor (bimap, first)

import Frontend.Desugaring.Final.Ast (Module(..))
import Frontend.Inference.Base.Common
import Frontend.Inference.Base.DebugOutput
import Frontend.Inference.BuiltIns
import qualified Frontend.Inference.InferenceProcessor as I
import qualified Frontend.Inference.Kind.Base as Kind
import Frontend.Inference.Signature
import qualified Frontend.Inference.Type.Processor as Type
import Frontend.Inference.TypeSynonyms.Processor

-- | Errors which can be encounterd during inference
data CombinedInferenceError
    = CombinedInferenceErrorKindInference I.InferenceError -- ^ Kind inference error
    | CombinedInferenceErrorTypeSynonyms TypeSynonymsProcessingError -- ^ An error of type synonyms expanding
    | CombinedInferenceErrorTypeInference InferenceError -- ^ Type inference error
    deriving (Eq, Show)

-- | Infer kinds of types in a module
inferKinds ::
       Module
    -> Signatures TypeConstructorSignature
    -> ( Either CombinedInferenceError (Signatures TypeConstructorSignature)
       , I.InferenceDebugOutput Kind.KindInferenceEnvironmentItem TypeConstructorSignature)
inferKinds module' state =
    first (first CombinedInferenceErrorKindInference) $
    Kind.inferKindsOfModule state module'

-- | Output of a step that expands type synonyms
newtype ExpandTypeSynonymsOutput = ExpandTypeSynonymsOutput
    { getExpandTypeSynonymSignatures :: TypeSynonymSignatures -- ^ Get expanded signatures
    } deriving (Eq, Show)

-- | Expend defined type synonyms
expandTypeSynonyms ::
       Module
    -> Signatures TypeConstructorSignature
    -> TypeSynonymSignatures
    -> Either CombinedInferenceError ExpandTypeSynonymsOutput
expandTypeSynonyms module' signatures initialSignatures =
    bimap CombinedInferenceErrorTypeSynonyms ExpandTypeSynonymsOutput $
    processSignatures
        initialSignatures
        (getModuleTypeSynonyms module')
        signatures

-- | Infers types of a modules
inferTypes ::
       Module
    -> Signatures TypeConstructorSignature
    -> TypeSynonymSignatures
    -> Type.TypeSignatures
    -> ( Either CombinedInferenceError Type.TypeSignatures
       , Type.TypeInferenceDebugOutput)
inferTypes module' signatures typeSynonyms initial =
    first (first CombinedInferenceErrorTypeInference) $
    Type.inferTypes signatures typeSynonyms initial module'
