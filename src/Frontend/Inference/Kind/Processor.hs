{- |
Module      :  Frontend.Inference.Kind.Processor
Description :  Functions for kind inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of kind inference
-}
module Frontend.Inference.Kind.Processor where

import Data.Bifunctor (first)
import qualified Data.HashMap.Lazy as HM

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Base.Common
import Frontend.Inference.Base.DebugOutput
import Frontend.Inference.Base.Descriptor
import Frontend.Inference.Base.Processor
import Frontend.Inference.Kind.Environment
import Frontend.Inference.Kind.Equalities
import Frontend.Inference.Kind.WithDependencies
import Frontend.Inference.Signature
import Frontend.Inference.Solver
import Frontend.Inference.Variables

-- | Infers kinds of data types, type synonyms and classes
inferKinds ::
       F.DataTypes
    -> F.TypeSynonyms
    -> F.Classes
    -> Signatures TypeConstructorSignature
    -> ( Either InferenceError (Signatures TypeConstructorSignature)
       , InferenceDebugOutput)
inferKinds dataTypes typeSynonyms classes initialState =
    let environment =
            Environment
                { getDataTypes = dataTypes
                , getTypeSynonyms = typeSynonyms
                , getClasses = classes
                }
        inferenceEnvironment =
            InferenceEnvironment
                { getInferenceEnvironmentSignatures = initialState
                , getInferenceEnvironmentTypeVariables = HM.empty
                }
     in first (fmap (\(x, _, _) -> x)) $
        runInfer
            kindInferenceDescriptor
            inferenceEnvironment
            emptyVariableGeneratorState
            environment

-- | Describes the process of kind inference
kindInferenceDescriptor ::
       InferenceDescriptor Environment (Signatures TypeConstructorSignature)
kindInferenceDescriptor =
    InferenceDescriptor
        { getInferenceDescriptorSignaturesGetter =
              const (Right HM.empty, mempty)
        , getInferenceDescriptorDependenyGraphBuilder =
              const getModuleDependencyGraph
        , getInferenceDescriptorSingleGroup =
              SingleGroupInferenceDescriptor
                  { getSingleGroupInferenceDescriptorEqualitiesBuilder =
                        generateEqualitiesForGroup
                  , getSingleGroupInferenceDescriptorApplySolution =
                        \e s -> HM.map (applyKindSolution e s)
                  }
        }
