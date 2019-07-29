{- |
Module      :  Frontend.Inference.Base.SingleGroupProcessor
Description :  Processor of inference in a single dependency group
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base processor of type and kind inference in a single dependency group
-}
module Frontend.Inference.Base.SingleGroupProcessor where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Control.Monad.Trans.Writer.Lazy (Writer, runWriter, tell)
import Data.Bifunctor (first, second)

import Frontend.Inference.Base.Common
import Frontend.Inference.Base.DebugOutput
import Frontend.Inference.Base.Descriptor
import Frontend.Inference.Solver
import Frontend.Inference.Variables

-- | A processor of inference of a single dependency group
type SingleGroupInferenceProcessor
     = ExceptT InferenceError (Writer [SingleGroupInferenceDebugOutput])

-- | Runs a processor
runSingleGroupInferenceProcessor ::
       SingleGroupInferenceProcessor a
    -> (Either InferenceError a, [SingleGroupInferenceDebugOutput])
runSingleGroupInferenceProcessor = runWriter . runExceptT

-- | Runs a processor and merges debug outputs
runSingleGroupInferenceProcessor' ::
       SingleGroupInferenceProcessor a
    -> (Either InferenceError a, SingleGroupInferenceDebugOutput)
runSingleGroupInferenceProcessor' =
    second mconcat . runSingleGroupInferenceProcessor

-- | Does inference of a single group
doInferSingleGroup ::
       (Semigroup s)
    => SingleGroupInferenceDescriptor a s
    -> InferenceEnvironment s
    -> Infer a s
    -> a
    -> InferenceState s
    -> [Ident]
    -> SingleGroupInferenceProcessor (InferenceState s)
doInferSingleGroup descr env infer x state group
    | InferenceState { getInferenceStateSignatures = signatures
                     , getInferenceStateVariableGeneratorState = varState
                     , getInferenceStateSolutions = solutions
                     } <- state =
        let allSignatures = getInferenceEnvironmentSignatures env <> signatures
            newEnv = env {getInferenceEnvironmentSignatures = allSignatures}
            processor = inferSingleGroup descr newEnv infer x group varState
            (result, output) = runSingleGroupInferenceProcessor' processor
         in do writeGroupDebugOutput output
               (newSignatures, newVarState, solution) <- except result
               return
                   InferenceState
                       { getInferenceStateSignatures =
                             signatures <> newSignatures
                       , getInferenceStateVariableGeneratorState = newVarState
                       , getInferenceStateSolutions = solutions ++ [solution]
                       }

-- | Infers a single group
inferSingleGroup ::
       SingleGroupInferenceDescriptor a s
    -> InferenceEnvironment s
    -> Infer a s
    -> a
    -> [Ident]
    -> VariableGeneratorState
    -> SingleGroupInferenceProcessor (SingleGroupInferenceOutput s)
inferSingleGroup descr env runInfer x group variableGeneratorState
    | SingleGroupInferenceDescriptor { getSingleGroupInferenceDescriptorEqualitiesBuilder = buildEqualities
                                     , getSingleGroupInferenceDescriptorApplySolution = applySolution
                                     } <- descr = do
        writeGroupDebugOutput $
            mempty {getSingleGroupInferenceDebugOutputGroup = Just group}
        -- Build equalities for each group
        let ((generated, nestedDebugOutput), newVariableGeneratorState) =
                buildEqualities runInfer env x group variableGeneratorState
        -- Save possible nested debug outputs
        writeGroupDebugOutput $
            mempty
                { getSingleGroupInferenceDebugOutputNested =
                      Just nestedDebugOutput
                }
        -- Generate equalities
        (signatures, equalities) <-
            wrapGroupError InferenceErrorEqualityGeneration generated
        -- Solve equalities
        (solution, solverDebugOutput) <-
            wrapGroupError InferenceErrorUnification $
            solveEqualities equalities
        writeGroupDebugOutput $
            mempty
                { getSingleGroupInferenceDebugOutputSolver =
                      Just solverDebugOutput
                }
        -- Apply solution to signatures
        let finalSignatures = applySolution equalities solution signatures
            output = (finalSignatures, newVariableGeneratorState, solution)
        return output

-- | Wraps an error, encountered during inference
wrapGroupError ::
       (a -> InferenceError) -> Either a b -> SingleGroupInferenceProcessor b
wrapGroupError f = except . first f

-- | Writes a debug output of a group
writeGroupDebugOutput ::
       SingleGroupInferenceDebugOutput -> SingleGroupInferenceProcessor ()
writeGroupDebugOutput = lift . tell . return
