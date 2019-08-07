{- |
Module      :  Frontend.Inference.Base.Processor
Description :  Processor of inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base processor of type and kind inference
-}
module Frontend.Inference.Base.Processor where

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Control.Monad.Trans.Writer.Lazy (Writer, runWriter, tell)
import Data.Bifunctor (first, second)
import qualified Data.HashSet as HS

import Frontend.Inference.Base.Common
import Frontend.Inference.Base.DebugOutput
import Frontend.Inference.Base.Descriptor
import Frontend.Inference.Base.SingleGroupProcessor
import Frontend.Inference.Base.Variables
import Frontend.Inference.DependencyResolver
import Frontend.Inference.Variables

-- | A processor of inference
type InferenceProcessor = ExceptT InferenceError (Writer [InferenceDebugOutput])

-- | Runs inference
runInfer :: RunInfer a s x
runInfer descr env variableState x =
    let inferenceProcessor = infer descr env variableState x
     in second mconcat . runWriter . runExceptT $ inferenceProcessor

-- | Does inference
infer ::
       InferenceDescriptor a s x
    -> InferenceEnvironment s
    -> VariableGeneratorState
    -> a
    -> InferenceProcessor (Signatures (x, s), VariableGeneratorState, TypeVariableEqualitiesMap)
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
        let (inferSignatures, signaturesDebugOutput) = getSignatures x
        writeDebugOutput
            mempty
                {getInferenceDebugOutputSignatures = Just signaturesDebugOutput}
        signatures <- except inferSignatures
        -- Build dependency graph, excluding expressions with explicit signatures
        let knownSignatures = definedSignatures <> signatures
            dependencyGraph = buildDependencyGraph knownSignatures x
        writeDebugOutput
            mempty
                {getInferenceDebugOutputDependencyGraph = Just dependencyGraph}
        -- Order dependency groups
        groups <-
            wrapError InferenceErrorDependencyResolution $
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
            inferGroups =
                foldM
                    inferenceStep
                    initialInferenceState
                    (map HS.toList $ reverse groups)
            (result, groupOutputs) =
                runSingleGroupInferenceProcessor inferGroups
        writeDebugOutput
            mempty
                { getInferenceDebugOutputDependencyGroupOutputs =
                      Just groupOutputs
                }
        -- Aggregate state of the inference
        InferenceState { getInferenceStateSignatures = newSignatures
                       , getInferenceStateVariableGeneratorState = newVariableGeneratorState
                       , getInferenceStateSolutions = solutions
                       } <- except result
        let typeVariableEqualities =
                collectTypeVariableEqualities definedTypeVariables solutions
        writeDebugOutput
            mempty
                { getInferenceDebugOutputTypeVariableEqualities =
                      Just typeVariableEqualities
                }
        return
            (newSignatures, newVariableGeneratorState, typeVariableEqualities)

-- | Wraps an error, encountered during inference
wrapError :: (a -> InferenceError) -> Either a b -> InferenceProcessor b
wrapError f = except . first f

-- | Writes a debug output
writeDebugOutput :: InferenceDebugOutput -> InferenceProcessor ()
writeDebugOutput = lift . tell . return
