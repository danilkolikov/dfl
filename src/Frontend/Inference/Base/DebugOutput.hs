{- |
Module      :  Frontend.Inference.Base.DebugOutput
Description :  Debug output of inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Debug output of inference
-}
module Frontend.Inference.Base.DebugOutput where

import Control.Applicative ((<|>))
import qualified Data.HashSet as HS

import Frontend.Inference.Base.Variables
import Frontend.Inference.DependencyResolver
import Frontend.Inference.Solver
import Frontend.Inference.Variables

-- | A debug output of inference
data InferenceDebugOutput = InferenceDebugOutput
    { getInferenceDebugOutputSignatures :: Maybe SingleGroupInferenceDebugOutput -- ^ Output of signatures inference
    , getInferenceDebugOutputDependencyGraph :: Maybe DependencyGraph -- ^ Dependency graph
    , getInferenceDebugOutputDependencyGroups :: Maybe [HS.HashSet Ident] -- ^ Dependency groups
    , getInferenceDebugOutputDependencyGroupOutputs :: Maybe [SingleGroupInferenceDebugOutput] -- ^ Processed dependency groups
    , getInferenceDebugOutputTypeVariableEqualities :: Maybe TypeVariableEqualitiesMap -- ^ Equalities between bound variables
    }

instance Semigroup InferenceDebugOutput where
    InferenceDebugOutput s1 gr1 g1 o1 e1 <> InferenceDebugOutput s2 gr2 g2 o2 e2 =
        InferenceDebugOutput
            (s1 <|> s2)
            (gr1 <|> gr2)
            (g1 <|> g2)
            (o1 <|> o2)
            (e1 <|> e2)

instance Monoid InferenceDebugOutput where
    mempty =
        InferenceDebugOutput
            { getInferenceDebugOutputSignatures = Nothing
            , getInferenceDebugOutputDependencyGraph = Nothing
            , getInferenceDebugOutputDependencyGroups = Nothing
            , getInferenceDebugOutputDependencyGroupOutputs = Nothing
            , getInferenceDebugOutputTypeVariableEqualities = Nothing
            }

-- | A debug output of inference of a single dependency group
data SingleGroupInferenceDebugOutput = SingleGroupInferenceDebugOutput
    { getSingleGroupInferenceDebugOutputGroup :: Maybe [Ident] -- ^ List of idents in a group
    , getSingleGroupInferenceDebugOutputNested :: Maybe [InferenceDebugOutput] -- ^ Nested outputs
    , getSingleGroupInferenceDebugOutputSolver :: Maybe SolverDebugOutput -- ^ Debug output of a solver
    }

instance Semigroup SingleGroupInferenceDebugOutput where
    (SingleGroupInferenceDebugOutput g1 n1 so1) <> (SingleGroupInferenceDebugOutput g2 n2 so2) =
        SingleGroupInferenceDebugOutput (g1 <|> g2) (n1 <|> n2) (so1 <|> so2)

instance Monoid SingleGroupInferenceDebugOutput where
    mempty =
        SingleGroupInferenceDebugOutput
            { getSingleGroupInferenceDebugOutputGroup = Nothing
            , getSingleGroupInferenceDebugOutputNested = Nothing
            , getSingleGroupInferenceDebugOutputSolver = Nothing
            }
