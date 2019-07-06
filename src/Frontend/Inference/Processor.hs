{- |
Module      :  Frontend.Inference.Processor
Description :  Processors of kind and type inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processors of kind and type inference
-}
module Frontend.Inference.Processor
    ( InferenceError(..)
    , KindInferenceState(..)
    , Kind.KindInferenceOutput(..)
    , Kind.KindInferenceGroupOutput(..)
    , Kind.KindInferenceError(..)
    , emptyKindInferenceState
    , inferKinds
    ) where

import Data.Bifunctor (first)

import Frontend.Desugaring.Final.Ast (Module(..))
import qualified Frontend.Inference.Kind.Processor as Kind
import Frontend.Inference.Kind.ProcessorBase

-- | Errors which can be encounterd during inference
data InferenceError =
    InferenceErrorKind Kind.KindInferenceError -- ^ Kind inference error
    deriving (Eq, Show)

-- | Infer kinds of types in a module
inferKinds ::
       Module -> KindInferenceState -> Either InferenceError Kind.KindInferenceOutput
inferKinds module' state =
    first InferenceErrorKind $
    Kind.inferKinds
        (getModuleDataTypes module')
        (getModuleTypeSynonyms module')
        (getModuleClasses module')
        state
