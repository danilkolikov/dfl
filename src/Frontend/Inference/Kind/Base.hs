{- |
Module      :  Frontend.Inference.Kind.Base
Description :  Base functions for kind inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base functions for kind inference
-}
module Frontend.Inference.Kind.Base
    ( KindInferenceEnvironmentItem(..)
    , KindInferenceEnvironment
    , inferKindsOfModule
    , checkKindOfObject
    , checkKindsOfExpressions
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

-- | Infers kinds of data types, type synonyms and classes of a single module
inferKindsOfModule ::
       Signatures TypeConstructorSignature
    -> F.Module
    -> ( Either InferenceError (Signatures TypeConstructorSignature)
       , InferenceDebugOutput KindInferenceEnvironmentItem TypeConstructorSignature)
inferKindsOfModule initialState F.Module { F.getModuleDataTypes = dataTypes
                                 , F.getModuleTypeSynonyms = typeSynonyms
                                 , F.getModuleClasses = classes
                                 } =
    let environment = prepareEnvironment typeSynonyms dataTypes classes
        applySolution result solution =
            HM.map (`applySolutionToSingleResult` solution) result
     in evalVariableGenerator . runWithDebugOutputT $
        inferMultipleGroups
            buildDependencyGraph
            (buildEqualities generateEqualitiesForGroup applySolution)
            initialState
            environment

-- | Checks kinds of a provided object
checkKindOfObject ::
       (WithEqualitiesAndSignature a)
    => Signatures TypeConstructorSignature
    -> a
    -> ( Either InferenceError TypeConstructorSignature
       , SingleGroupInferenceDebugOutput a TypeConstructorSignature)
checkKindOfObject knownSignatures input =
    evalVariableGenerator . runWithDebugOutputT $
    inferSingleGroup
        (buildEqualities
             generateEqualitiesAndSignature
             applySolutionToSingleResult)
        knownSignatures
        input

-- | Check kinds of provided expressions
checkKindsOfExpressions ::
       (WithEqualities a)
    => Signatures TypeConstructorSignature
    -> [a]
    -> (Either InferenceError [()], SingleGroupInferenceDebugOutput [a] [()])
checkKindsOfExpressions knownSignatures input =
    evalVariableGenerator . runWithDebugOutputT $
    inferSingleGroup
        (buildEqualitiesForChecking generateEqualitiesForList)
        knownSignatures
        input

-- | Builds dependency graph for provided environment items
buildDependencyGraph ::
       DependencyGraphBuilder KindInferenceEnvironmentItem TypeConstructorSignature
buildDependencyGraph = const getModuleDependencyGraph

-- | Generates equalities for a single group
buildEqualities ::
       BaseEqualitiesGeneratorFunction a b
    -> (b -> Solution -> c)
    -> EqualitiesBuilder a TypeConstructorSignature c
buildEqualities genEqualities applySolution signatures items =
    let buildEqualitiesAndApplySolution = do
            result <- genEqualities items
            return $ applySolution result
     in runEqualitiesGenerator'
            buildEqualitiesAndApplySolution
            emptyEqualitiesGeneratorEnvironment
                {getTypeConstructorSignatures = signatures}

-- | Applies solution of the system of type equations to the single result of generation of equalities
applySolutionToSingleResult ::
       (TypeConstructorSignature, [Ident])
    -> Solution
    -> TypeConstructorSignature
applySolutionToSingleResult (signature, params) solution =
    let boundVariables = HS.empty
     in applyKindSolutionAndSetTypeVariables
            params
            boundVariables
            solution
            signature

-- | Generates equalities for a single group, without substituting results
buildEqualitiesForChecking ::
       BaseEqualitiesGeneratorFunction a b
    -> EqualitiesBuilder a TypeConstructorSignature b
buildEqualitiesForChecking genEqualities signatures items =
    let buildEqualitiesAndApplySolution = const <$> genEqualities items
     in runEqualitiesGenerator'
            buildEqualitiesAndApplySolution
            emptyEqualitiesGeneratorEnvironment
                {getTypeConstructorSignatures = signatures}
