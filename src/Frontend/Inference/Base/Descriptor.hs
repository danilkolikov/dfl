{- |
Module      :  Frontend.Inference.Base.Descriptor
Description :  Description of inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Description of some kind of inference
-}
module Frontend.Inference.Base.Descriptor where

import qualified Data.HashSet as HS

import Frontend.Inference.Base.Common
import Frontend.Inference.Base.DebugOutput
import Frontend.Inference.Base.Variables
import Frontend.Inference.DependencyResolver
import Frontend.Inference.Equalities
import Frontend.Inference.Solver
import Frontend.Inference.Variables

-- | A function which infers explicit signatures
type SignaturesGetter a s
     = a -> ( Either InferenceError (Signatures s)
            , SingleGroupInferenceDebugOutput)

-- | A function which builds a dependency graph
type DependencyGraphBuilder a s = Signatures s -> a -> DependencyGraph

-- | An output of inference
type InferOutput s
     = ( Either InferenceError ( Signatures s
                               , VariableGeneratorState
                               , TypeVariableEqualitiesMap)
       , InferenceDebugOutput)

-- | A function that does inference
type Infer a s x
     = InferenceEnvironment s -> VariableGeneratorState -> a -> InferOutput (x, s)

-- | A function that runs inference
type RunInfer a s x = InferenceDescriptor a s x -> Infer a s x

-- | An output of a function which creates a system of equalities
type EqualitiesBuilderOutput s
     = ( ( Either InferenceEqualitiesGenerationError ( Signatures ( s
                                                                  , [Ident])
                                                     , Equalities)
         , [InferenceDebugOutput])
       , VariableGeneratorState)

-- | A function which creates a system of equalities
type EqualitiesBuilder a s x
     = Infer a s x -> InferenceEnvironment s -> a -> [Ident] -> VariableGeneratorState -> EqualitiesBuilderOutput (x, s)

-- | A function which applies a solution of a system of equalities
type SolutionApplier s = [Ident] -> HS.HashSet Ident -> Solution -> s -> s

-- | A structure that describes a process of inference
data InferenceDescriptor a s x = InferenceDescriptor
    { getInferenceDescriptorSignaturesGetter :: SignaturesGetter a s
    , getInferenceDescriptorDependenyGraphBuilder :: DependencyGraphBuilder a s
    , getInferenceDescriptorSingleGroup :: SingleGroupInferenceDescriptor a s x
    }

-- | A structure that describes a process of inference of a single dependency group
data SingleGroupInferenceDescriptor a s x = SingleGroupInferenceDescriptor
    { getSingleGroupInferenceDescriptorEqualitiesBuilder :: EqualitiesBuilder a s x
    , getSingleGroupInferenceDescriptorApplySolution :: SolutionApplier (x, s)
    }

-- | A generator of equalities
type InferenceEqualitiesGenerator
     = EqualitiesGenerator [InferenceDebugOutput] InferenceError
