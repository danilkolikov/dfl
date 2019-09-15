{- |
Module      :  Frontend.Inference.Kind.Processor
Description :  Functions for kind inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of kind inference
-}
module Frontend.Inference.Kind.Processor
    ( KindInferenceEnvironmentItem(..)
    , KindInferenceEnvironment
    , inferKinds
    , checkKindsOfTypeSignatures
    ) where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Equalities
import Frontend.Inference.InferenceProcessor
import Frontend.Inference.Kind.Environment
import Frontend.Inference.Kind.Equalities
import Frontend.Inference.Kind.WithDependencies
import Frontend.Inference.Signature
import Frontend.Inference.Solver
import Frontend.Inference.Util.Debug
import Frontend.Inference.Variables

-- | Infers kinds of data types, type synonyms and classes
inferKinds ::
       F.DataTypes
    -> F.TypeSynonyms
    -> F.Classes
    -> Signatures TypeConstructorSignature
    -> ( Either InferenceError (Signatures TypeConstructorSignature)
       , InferenceDebugOutput KindInferenceEnvironmentItem TypeConstructorSignature)
inferKinds dataTypes typeSynonyms classes initialState =
    let environment = prepareEnvironment typeSynonyms dataTypes classes
        variableGenerator =
            runWithDebugOutputT $
            inferMultipleGroups
                buildDependencyGraph
                (buildEqualities generateEqualitiesForGroup)
                initialState
                environment
     in evalVariableGenerator variableGenerator

-- | Checks kinds of provided type signatures
checkKindsOfTypeSignatures ::
       (WithEqualitiesAndSignature a)
    => Signatures TypeConstructorSignature
    -> Signatures a
    -> ( Either InferenceError (Signatures TypeConstructorSignature)
       , SingleGroupInferenceDebugOutput a TypeConstructorSignature)
checkKindsOfTypeSignatures knownSignatures input =
    let variableGenerator =
            runWithDebugOutputT $
            inferSingleGroup
                (buildEqualities generateEqualitiesForMap)
                knownSignatures
                input
     in evalVariableGenerator variableGenerator

-- | Builds dependency graph for provided environment items
buildDependencyGraph ::
       DependencyGraphBuilder KindInferenceEnvironmentItem TypeConstructorSignature
buildDependencyGraph = const getModuleDependencyGraph

-- | Generates equalities for a single group
buildEqualities ::
       EqualitiesGeneratorFunction a
    -> EqualitiesBuilder a TypeConstructorSignature
buildEqualities genEqualities signatures items =
    let boundVariables = HS.empty
        makeApplySolution solution (signature, params) =
            applyKindSolutionAndSetTypeVariables
                params
                boundVariables
                solution
                signature
        buildEqualitiesAndApplySolution = do
            result <- genEqualities items
            return $ \solution -> HM.map (makeApplySolution solution) result
     in runEqualitiesGenerator'
            buildEqualitiesAndApplySolution
            emptyEqualitiesGeneratorEnvironment
                {getTypeConstructorSignatures = signatures}
