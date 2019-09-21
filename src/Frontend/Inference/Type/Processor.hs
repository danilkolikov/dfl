{- |
Module      :  Frontend.Inference.Type.Processor
Description :  Base functions for type inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base functions for type inference
-}
module Frontend.Inference.Type.Processor
    ( inferTypesOfExpressions
    ) where

import qualified Data.HashMap.Lazy as HM

import Frontend.Inference.Equalities
import Frontend.Inference.InferenceProcessor
import qualified Frontend.Inference.Let.Ast as L
import Frontend.Inference.Signature
import Frontend.Inference.Solver
import qualified Frontend.Inference.Type.Ast as T
import Frontend.Inference.Type.Equalities
import Frontend.Inference.Type.WithDependencies
import Frontend.Inference.Util.Debug
import Frontend.Inference.Variables

-- | Infers types of provided expressions
inferTypesOfExpressions ::
       Signatures TypeSignature
    -> HM.HashMap Ident L.Expression
    -> ( Either InferenceError (Signatures (T.Exp, TypeSignature))
       , InferenceDebugOutput L.Expression (T.Exp, TypeSignature))
inferTypesOfExpressions initialState expressions =
    evalVariableGenerator . runWithDebugOutputT $
    inferMultipleGroupsGeneric
        snd
        buildDependencyGraph
        buildEqualities
        initialState
        expressions

-- | Builds dependency graph for provided expressions
buildDependencyGraph :: DependencyGraphBuilder L.Expression TypeSignature
buildDependencyGraph signatures =
    getExpressionsDependencyGraph (HM.keysSet signatures)

-- | Generates equalities for a single group
buildEqualities ::
       EqualitiesBuilder (HM.HashMap Ident L.Expression) TypeSignature (HM.HashMap Ident ( T.Exp
                                                                                         , TypeSignature))
buildEqualities signatures items =
    let buildEqualitiesAndApplySolution = do
            results <- generateEqualitiesForExpressions items
            return $ \solution -> HM.map (applySolution solution) results
     in runEqualitiesGenerator'
            buildEqualitiesAndApplySolution
            emptyEqualitiesGeneratorEnvironment
                {getExpressionSignatures = signatures}

-- | Applies solution of the system of type equations to the single result of generation of equalities
applySolution :: Solution -> (T.Exp, TypeSignature) -> (T.Exp, TypeSignature)
applySolution sol (exp', typeSig) =
    let appliedExp = applyTypeSolution sol exp'
        appliedTypeSig = applyTypeSolutionAndGeneralise sol typeSig
        constraints = getSolutionTypeConstraints sol
        finalTypeSig = appliedTypeSig {getTypeSignatureContext = constraints}
     in (appliedExp, finalTypeSig)
