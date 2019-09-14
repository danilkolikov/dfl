{- |
Module      :  Frontend.Inference.Base.SingleGroupProcessor
Description :  Processor of inference in a single dependency group
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base processor of type and kind inference in a single dependency group
-}
module Frontend.Inference.Base.SingleGroupProcessor where

import qualified Data.HashMap.Lazy as HM

import Frontend.Inference.Base.Common
import Frontend.Inference.Base.DebugOutput
import Frontend.Inference.Base.Descriptor
import Frontend.Inference.Solver
import Frontend.Inference.Util.Debug
import Frontend.Inference.Variables

-- | A processor of inference of a single dependency group
type SingleGroupInferenceProcessor
     = WithDebugOutput InferenceError SingleGroupInferenceDebugOutput

-- | Does inference of a single group
doInferSingleGroup ::
       SingleGroupInferenceDescriptor a s x
    -> InferenceEnvironment s
    -> Infer a s x
    -> a
    -> InferenceState (x, s)
    -> [Ident]
    -> SingleGroupInferenceProcessor (InferenceState (x, s))
doInferSingleGroup descr env infer x state group
    | InferenceState { getInferenceStateSignatures = signatures
                     , getInferenceStateVariableGeneratorState = varState
                     , getInferenceStateSolutions = solutions
                     } <- state = do
        let allSignatures =
                getInferenceEnvironmentSignatures env <> HM.map snd signatures
            newEnv = env {getInferenceEnvironmentSignatures = allSignatures}
            processor = inferSingleGroup descr newEnv infer x group varState
        (newSignatures, newVarState, solution) <- processor
        return
            InferenceState
                { getInferenceStateSignatures = signatures <> newSignatures
                , getInferenceStateVariableGeneratorState = newVarState
                , getInferenceStateSolutions = solutions ++ [solution]
                }

-- | Infers a single group
inferSingleGroup ::
       SingleGroupInferenceDescriptor a s x
    -> InferenceEnvironment s
    -> Infer a s x
    -> a
    -> [Ident]
    -> VariableGeneratorState
    -> SingleGroupInferenceProcessor (SingleGroupInferenceOutput (x, s))
inferSingleGroup descr env runInfer x group variableGeneratorState
    | SingleGroupInferenceDescriptor { getSingleGroupInferenceDescriptorEqualitiesBuilder = buildEqualities
                                     , getSingleGroupInferenceDescriptorApplySolution = applySolution
                                     } <- descr = do
        writeDebugOutput
            mempty {getSingleGroupInferenceDebugOutputGroup = Just group}
        -- Build equalities for each group
        let (buildResult, newVariableGeneratorState) =
                buildEqualities runInfer env x group variableGeneratorState
        -- Save possible nested debug outputs
        (signatures, equalities) <-
            wrapEither
                InferenceErrorEqualityGeneration
                buildResult
        -- Solve equalities
        solution <-
            wrapErrorAndDebugOutput
                InferenceErrorUnification
                (\debug ->
                     mempty
                         {getSingleGroupInferenceDebugOutputSolver = Just debug}) $
            solveEqualities equalities
        -- Apply solution to signatures
        let boundVariables =
                HM.keysSet . getInferenceEnvironmentTypeVariables $ env
            finalSignatures =
                HM.map
                    (\(s, vars) -> applySolution vars boundVariables solution s)
                    signatures
        return (finalSignatures, newVariableGeneratorState, solution)
