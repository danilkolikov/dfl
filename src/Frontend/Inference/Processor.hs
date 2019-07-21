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
    , Type.TypeSignatures(..)
    , TypeSynonymSignatures
    , Kind.KindInferenceDebugOutput(..)
    , Kind.KindInferenceGroupDebugOutput(..)
    , Kind.KindInferenceError(..)
    , Type.TypeInferenceError(..)
    , ExpandTypeSynonymsOutput(..)
    , TypeSynonymsProcessingError(..)
    , Equalities(..)
    , DependencyGroupItem(..)
    , DependencyGroupItemEmpty
    , DependencyGroupItemWithSignature
    , emptySignatures
    , Type.emptyTypeSignatures
    , inferKinds
    , expandTypeSynonyms
    , inferTypes
    ) where

import Data.Bifunctor (bimap, first)

import Frontend.Desugaring.Final.Ast (Module(..))
import qualified Frontend.Inference.Kind.Processor as Kind
import Frontend.Inference.Kind.ProcessorBase
import qualified Frontend.Inference.Type.Processor as Type
import Frontend.Inference.TypeSynonyms.Processor

-- | Errors which can be encounterd during inference
data InferenceError
    = InferenceErrorKind Kind.KindInferenceError -- ^ A kind inference error
    | InferenceErrorTypeSynonyms TypeSynonymsProcessingError -- ^ An error of type synonyms expanding
    | InferenceErrorType Type.TypeInferenceError -- ^ A type inference error
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

-- | Infers types of a modules
inferTypes ::
       Module
    -> Signatures
    -> TypeSynonymSignatures
    -> Type.TypeSignatures
    -> Either InferenceError Type.TypeSignatures
inferTypes module' signatures typeSynonyms initial =
    first InferenceErrorType $
    Type.inferTypes module' signatures typeSynonyms initial
