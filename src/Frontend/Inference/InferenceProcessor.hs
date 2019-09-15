{- |
Module      :  Frontend.Inference.InferenceProcessor
Description :  A non-recursive generic processor of inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

A non-recursive generic processor of type and kind inference
-}
module Frontend.Inference.InferenceProcessor
    ( InferenceError(..)
    , Signatures
    , InferenceProcessor
    , InferenceDebugOutput(..)
    , SingleGroupInferenceProcessor
    , SingleGroupInferenceDebugOutput(..)
    , EqualitiesBuilderOutput
    , EqualitiesBuilder
    , DependencyGraphBuilder
    , inferMultipleGroups
    , inferSingleGroup
    ) where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Frontend.Inference.Base.Common (InferenceError(..), Signatures)
import Frontend.Inference.DependencyResolver
import Frontend.Inference.Equalities
import Frontend.Inference.Solver
import Frontend.Inference.Util.Debug
import Frontend.Inference.Variables

-- | A processor of inference of multiple groups
type InferenceProcessor a s
     = WithDebugOutputT InferenceError (InferenceDebugOutput a s) VariableGenerator

-- | A debug output of inference of multiple groups
data InferenceDebugOutput a s = InferenceDebugOutput
    { getInferenceDebugOutputInput :: Maybe (HM.HashMap Ident a) -- ^ Input of inference
    , getInferenceDebugOutputDependencyGraph :: Maybe DependencyGraph -- ^ Dependency graph
    , getInferenceDebugOutputDependencyGroups :: Maybe [HS.HashSet Ident] -- ^ Dependency groups
    , getInferenceDebugOutputDependencyGroupOutputs :: Maybe [SingleGroupInferenceDebugOutput (HM.HashMap Ident a) (HM.HashMap Ident s)] -- ^ Processed dependency groups
    , getInferenceDebugOutputSignatures :: Maybe (HM.HashMap Ident s) -- ^ Output signatures
    }

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
    }

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
type EqualitiesBuilderOutput s
     = Either EqualitiesGenerationError (Solution -> s, Equalities)

-- | A function which builds a system of equalities
type EqualitiesBuilder a s o
     = Signatures s -> a -> VariableGenerator (EqualitiesBuilderOutput o)

-- | A function which builds a dependency graph
type DependencyGraphBuilder a s
     = Signatures s -> HM.HashMap Ident a -> DependencyGraph

-- | Infer signatures of multiple (possibly mutually dependent) groups
inferMultipleGroups ::
       DependencyGraphBuilder a s
    -> EqualitiesBuilder (HM.HashMap Ident a) s (Signatures s)
    -> Signatures s
    -> HM.HashMap Ident a
    -> InferenceProcessor a s (Signatures s)
inferMultipleGroups buildDependencyGraph buildEqualities definedSignatures groups = do
    writeDebugOutput mempty {getInferenceDebugOutputInput = Just groups}
    -- Get a dependency graph
    let dependencyGraph = buildDependencyGraph definedSignatures groups
    writeDebugOutput
        mempty {getInferenceDebugOutputDependencyGraph = Just dependencyGraph}
    -- Find dependency groups and infer each one
    let buildEqualitiesWithSignatures extraSignatures =
            buildEqualities (definedSignatures <> extraSignatures)
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
    let finalSignatures = getFinalSignatures solution
    writeDebugOutput
        mempty
            { getSingleGroupInferenceDebugOutputSignatures =
                  Just finalSignatures
            }
    return finalSignatures
