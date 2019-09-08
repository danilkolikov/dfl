{- |
Module      :  Frontend.Inference.Base.Processor
Description :  Processor of inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base processor of type and kind inference
-}
module Frontend.Inference.Base.Processor where

import Data.Bifunctor (second)
import qualified Data.HashSet as HS

import Frontend.Inference.Base.Common
import Frontend.Inference.Base.DebugOutput
import Frontend.Inference.Base.Descriptor
import Frontend.Inference.Base.SingleGroupProcessor
import Frontend.Inference.Base.Variables
import Frontend.Inference.DependencyResolver
import Frontend.Inference.Util.Debug
import Frontend.Inference.Variables

-- | A processor of inference
type InferenceProcessor = WithDebugOutput InferenceError InferenceDebugOutput

-- | Runs inference
runInfer :: RunInfer a s x
runInfer descr env variableState x =
    runWithDebugOutput $ infer descr env variableState x

-- | Does inference
infer ::
       InferenceDescriptor a s x
    -> InferenceEnvironment s
    -> VariableGeneratorState
    -> a
    -> InferenceProcessor ( Signatures (x, s)
                          , VariableGeneratorState
                          , TypeVariableEqualitiesMap)
infer descr env variableGeneratorState x
    | InferenceDescriptor { getInferenceDescriptorSignaturesGetter = getSignatures
                          , getInferenceDescriptorDependenyGraphBuilder = buildDependencyGraph
                          , getInferenceDescriptorSingleGroup = singleGroupDescr
                          } <- descr
    , InferenceEnvironment { getInferenceEnvironmentSignatures = definedSignatures
                           , getInferenceEnvironmentTypeVariables = definedTypeVariables
                           } <- env
        -- Check explicit signatures for expressions
     = do
        signatures <-
            wrapDebugOutput
                (\debug ->
                     mempty {getInferenceDebugOutputSignatures = Just debug}) $
            getSignatures x
        -- Build dependency graph, excluding expressions with explicit signatures
        let knownSignatures = definedSignatures <> signatures
            dependencyGraph = buildDependencyGraph knownSignatures x
        writeDebugOutput
            mempty
                {getInferenceDebugOutputDependencyGraph = Just dependencyGraph}
        -- Order dependency groups
        groups <-
            wrapEither InferenceErrorDependencyResolution $
            getDependencyGroups dependencyGraph
        writeDebugOutput
            mempty {getInferenceDebugOutputDependencyGroups = Just groups}
        -- Infer each group
        let newEnv = env {getInferenceEnvironmentSignatures = knownSignatures}
            initialInferenceState =
                InferenceState
                    { getInferenceStateSignatures = mempty
                    , getInferenceStateVariableGeneratorState =
                          variableGeneratorState
                    , getInferenceStateSolutions = []
                    }
            inferenceStep =
                doInferSingleGroup singleGroupDescr newEnv (runInfer descr) x
            inferGroups state [] = (Right state, [])
            inferGroups state (group:rest) =
                let (result, debug) =
                        runWithDebugOutput $ inferenceStep state group
                 in case result of
                        Left _ -> (result, [debug])
                        Right newState ->
                            second (debug :) $ inferGroups newState rest
        -- Aggregate state of the inference
        InferenceState { getInferenceStateSignatures = newSignatures
                       , getInferenceStateVariableGeneratorState = newVariableGeneratorState
                       , getInferenceStateSolutions = solutions
                       } <-
            wrapDebugOutput
                (\debug ->
                     mempty
                         { getInferenceDebugOutputDependencyGroupOutputs =
                               Just debug
                         }) $
            inferGroups initialInferenceState (map HS.toList $ reverse groups)
        let typeVariableEqualities =
                collectTypeVariableEqualities definedTypeVariables solutions
        writeDebugOutput
            mempty
                { getInferenceDebugOutputTypeVariableEqualities =
                      Just typeVariableEqualities
                }
        return
            (newSignatures, newVariableGeneratorState, typeVariableEqualities)
