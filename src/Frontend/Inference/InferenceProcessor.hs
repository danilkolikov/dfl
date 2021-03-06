{- |
Module      :  Frontend.Inference.InferenceProcessor
Description :  A non-recursive generic processor of inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

A non-recursive generic processor of type and kind inference
-}
module Frontend.Inference.InferenceProcessor
    ( InferenceError(..)
    , SignatureCheckError(..)
    , VariableBinding
    , Signatures
    , InferenceProcessor
    , InferenceDebugOutput(..)
    , SingleGroupInferenceProcessor
    , SingleGroupInferenceDebugOutput(..)
    , EqualitiesBuilderOutput
    , EqualitiesBuilder
    , DependencyGraphBuilder
    , inferMultipleGroups
    , inferMultipleGroupsGeneric
    , inferSingleGroup
    ) where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Frontend.Inference.Constraint (Constraint)
import Util.DependencyResolver
import Frontend.Inference.Equalities
import Frontend.Inference.Signature (Signatures)
import Frontend.Inference.Solver
import Frontend.Inference.Unification
import Util.Debug
import Frontend.Inference.Variables

-- | A processor of inference of multiple groups
type InferenceProcessor a o
     = WithDebugOutputT InferenceError (InferenceDebugOutput a o) VariableGenerator (Signatures o)

-- | A type of errors which may be encountered during type inference
data InferenceError
    = InferenceErrorDependencyResolution (DependencyResolverError Ident) -- ^ An error happened during resolution of dependency groups
    | InferenceErrorEqualityGeneration EqualitiesGenerationError -- ^ An error happened during generation of equalities for a single group
    | InferenceErrorUnification UnificationError -- ^ An error happened during unification
    | InferenceErrorSignatureCheck SignatureCheckError -- ^ An error of signature check
    deriving (Eq, Show)

-- | A type of error which may be encountered during check of explicit signatures
data SignatureCheckError
    = SignatureCheckErrorBoundVariable VariableBinding -- ^ A free variable was bound
    | SignatureCheckErrorUnexpectedConstraint Constraint -- ^ An unexpected type constraint was introduced
    deriving (Eq, Show)

-- | An unexpected binding of either kind or type variable
type VariableBinding = (Ident, Either Kind Type)

-- | A debug output of inference of multiple groups
data InferenceDebugOutput a s = InferenceDebugOutput
    { getInferenceDebugOutputInput :: Maybe (HM.HashMap Ident a) -- ^ Input of inference
    , getInferenceDebugOutputDependencyGraph :: Maybe (DependencyGraph Ident) -- ^ Dependency graph
    , getInferenceDebugOutputDependencyGroups :: Maybe [HS.HashSet Ident] -- ^ Dependency groups
    , getInferenceDebugOutputDependencyGroupOutputs :: Maybe [SingleGroupInferenceDebugOutput (HM.HashMap Ident a) (HM.HashMap Ident s)] -- ^ Processed dependency groups
    , getInferenceDebugOutputSignatures :: Maybe (HM.HashMap Ident s) -- ^ Output signatures
    } deriving (Eq, Show)

instance Semigroup (InferenceDebugOutput a s) where
    InferenceDebugOutput i1 gr1 g1 o1 sig1 <> InferenceDebugOutput i2 gr2 g2 o2 sig2 =
        InferenceDebugOutput
            (i1 <|> i2)
            (gr1 <|> gr2)
            (g1 <|> g2)
            (o1 <|> o2)
            (sig1 <|> sig2)

instance Monoid (InferenceDebugOutput a s) where
    mempty =
        InferenceDebugOutput
            { getInferenceDebugOutputInput = Nothing
            , getInferenceDebugOutputDependencyGraph = Nothing
            , getInferenceDebugOutputDependencyGroups = Nothing
            , getInferenceDebugOutputDependencyGroupOutputs = Nothing
            , getInferenceDebugOutputSignatures = Nothing
            }

-- | A processor of inference of a single dependency group
type SingleGroupInferenceProcessor a s
     = WithDebugOutputT InferenceError (SingleGroupInferenceDebugOutput a s) VariableGenerator

-- | A debug output of inference of a single dependency group
data SingleGroupInferenceDebugOutput a s = SingleGroupInferenceDebugOutput
    { getSingleGroupInferenceDebugOutputInput :: Maybe a -- ^ Current group
    , getSingleGroupInferenceDebugOutputSolver :: Maybe SolverDebugOutput -- ^ Debug output of a solver
    , getSingleGroupInferenceDebugOutputSignatures :: Maybe s -- ^ Inferred signatures
    } deriving (Eq, Show)

instance Semigroup (SingleGroupInferenceDebugOutput a s) where
    (SingleGroupInferenceDebugOutput i1 so1 sig1) <> (SingleGroupInferenceDebugOutput i2 so2 sig2) =
        SingleGroupInferenceDebugOutput
            (i1 <|> i2)
            (so1 <|> so2)
            (sig1 <|> sig2)

instance Monoid (SingleGroupInferenceDebugOutput a s) where
    mempty =
        SingleGroupInferenceDebugOutput
            { getSingleGroupInferenceDebugOutputInput = Nothing
            , getSingleGroupInferenceDebugOutputSolver = Nothing
            , getSingleGroupInferenceDebugOutputSignatures = Nothing
            }

-- | Output of a process of building of equalities
type EqualitiesBuilderOutput o
     = Either EqualitiesGenerationError ( Solution -> Either SignatureCheckError o
                                        , Equalities)

-- | A function which builds a system of equalities
type EqualitiesBuilder a s o
     = Signatures s -> a -> VariableGenerator (EqualitiesBuilderOutput o)

-- | A function which builds a dependency graph
type DependencyGraphBuilder a s
     = Signatures s -> HM.HashMap Ident a -> DependencyGraph Ident

-- | Infer signatures of multiple (possibly mutually dependent) groups
inferMultipleGroups ::
       DependencyGraphBuilder a s
    -> EqualitiesBuilder (HM.HashMap Ident a) s (Signatures s)
    -> Signatures s
    -> HM.HashMap Ident a
    -> InferenceProcessor a s
inferMultipleGroups = inferMultipleGroupsGeneric id

-- | Infer signatures of multiple (possibly mutually dependent) groups with generic output
inferMultipleGroupsGeneric ::
       (o -> s)
    -> DependencyGraphBuilder a s
    -> EqualitiesBuilder (HM.HashMap Ident a) s (Signatures o)
    -> Signatures s
    -> HM.HashMap Ident a
    -> InferenceProcessor a o
inferMultipleGroupsGeneric outputMapper buildDependencyGraph buildEqualities definedSignatures groups = do
    writeDebugOutput mempty {getInferenceDebugOutputInput = Just groups}
    -- Get a dependency graph
    let dependencyGraph = buildDependencyGraph definedSignatures groups
    writeDebugOutput
        mempty {getInferenceDebugOutputDependencyGraph = Just dependencyGraph}
    -- Find dependency groups and infer each one
    let buildEqualitiesWithSignatures extraSignatures =
            buildEqualities
                (definedSignatures <> HM.map outputMapper extraSignatures)
        selectGroup group = HM.filterWithKey (\k _ -> HS.member k group) groups
        inferenceStep state group =
            mapDebugOutput return $ do
                singleGroupResult <-
                    inferSingleGroup
                        buildEqualitiesWithSignatures
                        state
                        (selectGroup group)
                -- Concatenate initial and final states
                return $ state <> singleGroupResult
    -- Traverse the dependency graph
    (dependencyGroups, inferenceResult) <-
        wrapEither InferenceErrorDependencyResolution $
        traverseGraph inferenceStep HM.empty dependencyGraph
    writeDebugOutput
        mempty {getInferenceDebugOutputDependencyGroups = Just dependencyGroups}
    -- Get inferred signatures
    newSignatures <-
        mapDebugOutput
            (\debug ->
                 mempty
                     { getInferenceDebugOutputDependencyGroupOutputs =
                           Just debug
                     })
            inferenceResult
    writeDebugOutput
        mempty {getInferenceDebugOutputSignatures = Just newSignatures}
    return newSignatures

-- | Infer signatures of a single group
inferSingleGroup ::
       EqualitiesBuilder a s o
    -> Signatures s
    -> a
    -> SingleGroupInferenceProcessor a o o
inferSingleGroup buildEqualities knownSignatures group = do
    writeDebugOutput
        mempty {getSingleGroupInferenceDebugOutputInput = Just group}
    -- Build equalities for a group
    buildResult <- liftInner $ buildEqualities knownSignatures group
    (getFinalSignatures, equalities) <-
        wrapEither InferenceErrorEqualityGeneration buildResult
    -- Solve equalities
    solution <-
        wrapErrorAndDebugOutput
            InferenceErrorUnification
            (\debug ->
                 mempty {getSingleGroupInferenceDebugOutputSolver = Just debug}) $
        solveEqualities equalities
    -- Apply solution to get signatures
    finalSignatures <-
        wrapEither InferenceErrorSignatureCheck $ getFinalSignatures solution
    writeDebugOutput
        mempty
            { getSingleGroupInferenceDebugOutputSignatures =
                  Just finalSignatures
            }
    return finalSignatures
