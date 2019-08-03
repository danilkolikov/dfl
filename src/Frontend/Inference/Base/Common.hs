{- |
Module      :  Frontend.Inference.Base.Common
Description :  Common definitions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Common definitions
-}
module Frontend.Inference.Base.Common where

import qualified Data.HashMap.Lazy as HM

import Frontend.Inference.DependencyResolver
import Frontend.Inference.Equalities
import Frontend.Inference.Solver
import Frontend.Inference.TypeSynonyms.Expand
import Frontend.Inference.Unification
import Frontend.Inference.Variables

-- | A map of signatures
type Signatures s = HM.HashMap Ident s

-- | An environment of type inference
data InferenceEnvironment s = InferenceEnvironment
    { getInferenceEnvironmentSignatures :: Signatures s
    , getInferenceEnvironmentTypeVariables :: TypeVariables
    }

-- | A state of inference
data InferenceState s = InferenceState
    { getInferenceStateSignatures :: Signatures s
    , getInferenceStateVariableGeneratorState :: VariableGeneratorState
    , getInferenceStateSolutions :: [Solution]
    }

-- | An output of inference of a single group
type SingleGroupInferenceOutput s = (Signatures s, VariableGeneratorState, Solution)

-- | A type of errors which may be encountered during type inference
data InferenceError
    = InferenceErrorSynonyms TypeSynonymsExpandingError -- ^ An error happened during expansion of signatures
    | InferenceErrorDependencyResolution DependencyResolverError -- ^ An error happened during resolution of dependency groups
    | InferenceErrorEqualityGeneration InferenceEqualitiesGenerationError -- ^ An error happened during generation of equalities for a single group
    | InferenceErrorUnification UnificationError -- ^ An error happened during unification
    deriving (Eq, Show)

-- | An error of equalities generation
type InferenceEqualitiesGenerationError
     = EqualitiesGenerationError InferenceError
