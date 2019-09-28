{- |
Module      :  Frontend.Inference.Type.Processor
Description :  Base functions for type inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base functions for type inference
-}
module Frontend.Inference.Type.Processor
    ( inferTypesOfExpressions
    , TypeInferenceDebugOutput
    ) where

import Data.Bifunctor (second)
import qualified Data.HashMap.Lazy as HM
import Data.List (find)

import Frontend.Inference.Equalities
import qualified Frontend.Inference.Expression as T
import Frontend.Inference.InferenceProcessor
import qualified Frontend.Inference.Let.Ast as L
import Frontend.Inference.Signature
import Frontend.Inference.Solver
import Frontend.Inference.Substitution
import Frontend.Inference.Type.Equalities
import Frontend.Inference.Type.WithDependencies
import Util.Debug
import Frontend.Inference.Util.HashMap
import Frontend.Inference.Variables

-- | Debug output of type inference
type TypeInferenceDebugOutput
     = InferenceDebugOutput L.Expression (T.Exp, TypeSignature)

-- | Infers types of provided expressions
inferTypesOfExpressions ::
       Signatures TypeSignature
    -> HM.HashMap Ident L.Expression
    -> ( Either InferenceError (Signatures (T.Exp, TypeSignature))
       , TypeInferenceDebugOutput)
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
            return $ \solution -> mapHashMapM (applySolution solution) results
     in runEqualitiesGenerator'
            buildEqualitiesAndApplySolution
            emptyEqualitiesGeneratorEnvironment
                {getExpressionSignatures = signatures}

-- | Applies solution of the system of type equations to the single result of generation of equalities
applySolution ::
       Solution
    -> ( (T.Exp, TypeSignature)
       , (Substitution Type, Substitution Kind)
       , Maybe TypeSignature)
    -> Either SignatureCheckError (T.Exp, TypeSignature)
applySolution sol ((exp', typeSig), subs, expectedSig) =
    let appliedExp = applyTypeSolution sol exp'
        appliedTypeSig = applyTypeSolutionAndGeneralise sol typeSig
        constraints = getSolutionTypeConstraints sol
        finalTypeSig = appliedTypeSig {getTypeSignatureContext = constraints}
     in case expectedSig of
            Nothing -> return (appliedExp, finalTypeSig) -- No predefined type signature
            Just signature ->
                checkSignatures sol subs signature finalTypeSig >>
                return (appliedExp, signature)

checkSignatures ::
       Solution
    -> (Substitution Type, Substitution Kind)
    -> TypeSignature
    -> TypeSignature
    -> Either SignatureCheckError ()
checkSignatures solution (typeSub, kindSub) expected got
    | TypeSignature {getTypeSignatureContext = expectedContext} <- expected
    , TypeSignature {getTypeSignatureContext = gotContext} <- got
    , Solution { getSolutionTypeSubstitution = solutionTypeSub
               , getSolutionKindSubstitution = solutionKindSub
               } <- solution =
        let isTypeVariable type' =
                case type' of
                    TypeVar {} -> True
                    _ -> False
            isKindVariable kind =
                case kind of
                    KindVar {} -> True
                    _ -> False
         in do checkVariableBinding
                   isKindVariable
                   (second Left)
                   kindSub
                   solutionKindSub
               checkVariableBinding
                   isTypeVariable
                   (second Right)
                   typeSub
                   solutionTypeSub
               checkContexts expectedContext gotContext

checkVariableBinding ::
       (Substitutable a)
    => (a -> Bool)
    -> ((Ident, a) -> VariableBinding)
    -> Substitution a
    -> Substitution a
    -> Either SignatureCheckError ()
checkVariableBinding isVariable wrapper initialSub solutionSub =
    mapM_ checkParam $ HM.keys initialSub
  where
    composedSub = initialSub `compose` solutionSub
    checkParam name =
        case HM.lookup name composedSub of
            Nothing -> return ()
            Just found ->
                if isVariable found
                    then return ()
                    else Left . SignatureCheckErrorBoundVariable $
                         wrapper (name, found)

checkContexts :: [Constraint] -> [Constraint] -> Either SignatureCheckError ()
checkContexts expected got =
    case find (not . (`elem` expected)) got of
        Just unexpected ->
            Left $ SignatureCheckErrorUnexpectedConstraint unexpected
        Nothing -> return ()
