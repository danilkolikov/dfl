{- |
Module      :  Frontend.Inference.Kind.Processor
Description :  Functions for kind inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of kind inference
-}
module Frontend.Inference.Kind.Processor where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, except, runExcept)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.State.Lazy (StateT, get, put, runStateT)
import Data.Bifunctor (first)
import qualified Data.HashSet as HS

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.DependencyResolver
import Frontend.Inference.Kind.Ast
import Frontend.Inference.Kind.DependencyGroup
import Frontend.Inference.Kind.Kind
import Frontend.Inference.Kind.KindMapping
import Frontend.Inference.Kind.ProcessorBase
import Frontend.Inference.Kind.WithDependencies
import Frontend.Inference.Substitution
import Frontend.Inference.Unification

-- | Errors which may be encountered during kind inference
data KindInferenceError
    = KindInferenceErrorDependencyResolution DependencyResolverError -- ^ Error happened during dependency resolution
    | KindInferenceErrorDependencyGroupResolution DependencyGroupResolvingError -- ^ Error happened during resolution of a single group
    | KindInferenceErrorUnification UnificationError -- ^ Error happened during unification
    deriving (Eq, Show)

-- | Output of kind inference
data KindInferenceOutput = KindInferenceOutput
    { getKindInferenceOutputState :: KindInferenceState -- ^ State with resolved kinds
    , getKindInferenceOutputDependencies :: DependencyGraph -- ^ Dependency graph of a module
    , getKindInferenceOutputDependencyGroups :: [KindInferenceGroupOutput] -- ^ Processed dependency groups
    } deriving (Eq, Show)

-- | Output of inference of a single dependency group
data KindInferenceGroupOutput = KindInferenceGroupOutput
    { getKindInferenceGroupOutputIdents :: [Ident] -- ^ List of idents in a group
    , getKindInferenceGroupOutputEqualities :: KindEqualities -- ^ Equalities between kinds
    , getKindInferenceGroupOutputSolution :: Substitution Kind -- ^ Solution for the equalities
    } deriving (Eq, Show)

-- | Type of the processor of kind inference
type KindInferenceProcessor a
     = ReaderT Environment (Except KindInferenceError) a

-- | Infer kinds of data types, type synonyms and classes
inferKinds ::
       F.DataTypes
    -> F.TypeSynonyms
    -> F.Classes
    -> KindInferenceState
    -> Either KindInferenceError KindInferenceOutput
inferKinds dataTypes typeSynonyms classes initialState =
    let environment =
            Environment
                { getDataTypes = dataTypes
                , getTypeSynonyms = typeSynonyms
                , getClasses = classes
                }
     in runExcept (runReaderT (inferKinds' initialState) environment)

-- | Infer kinds of data types, type synonyms and classes
inferKinds' :: KindInferenceState -> KindInferenceProcessor KindInferenceOutput
inferKinds' initialState = do
    env <- ask
    let dependencyGraph = getModuleDependencyGraph env
        dependencyGroups = getDependencyGroups dependencyGraph
    groups <- lift . except $ wrapDependencyResolverError dependencyGroups
    let inferGroups = mapM (inferSingleGroup . HS.toList) groups
        inferredGroups =
            runExcept (runStateT (runReaderT inferGroups env) initialState)
    (groupOutputs, state) <- lift $ except inferredGroups
    return
        KindInferenceOutput
            { getKindInferenceOutputState = state
            , getKindInferenceOutputDependencies = dependencyGraph
            , getKindInferenceOutputDependencyGroups = groupOutputs
            }

-- | Processor of kind inference of a single dependency group
type SingleGroupInferenceProcessor a
     = ReaderT Environment (StateT KindInferenceState (Except KindInferenceError)) a

-- | Infer kinds of a single dependency group
inferSingleGroup ::
       [Ident] -> SingleGroupInferenceProcessor KindInferenceGroupOutput
inferSingleGroup group = do
    env <- ask
    state <- lift get
    let mappingsForIdents = createMappingsForIdents group env 0
        resolvedGroup = resolveDependencyGroup group state mappingsForIdents 0
    (groupWithKinds, equalities) <-
        lift . lift . except . wrapDependencyGroupResolvingError $ resolvedGroup
    let unifiedEqualities = unifyEqualities equalities
    substitution <-
        lift . lift . except . wrapUnificationError $ unifiedEqualities
    let inferredGroup = substituteKind substitution groupWithKinds
        newState = state <> inferredGroup
    lift $ put newState
    return
        KindInferenceGroupOutput
            { getKindInferenceGroupOutputIdents = group
            , getKindInferenceGroupOutputEqualities = equalities
            , getKindInferenceGroupOutputSolution = substitution
            }

-- | Wrap dependency resolver error
wrapDependencyResolverError ::
       Either DependencyResolverError a -> Either KindInferenceError a
wrapDependencyResolverError = first KindInferenceErrorDependencyResolution

-- | Wrap dependency group resolving error
wrapDependencyGroupResolvingError ::
       Either DependencyGroupResolvingError a -> Either KindInferenceError a
wrapDependencyGroupResolvingError =
    first KindInferenceErrorDependencyGroupResolution

-- | Wrap unification error
wrapUnificationError :: Either UnificationError a -> Either KindInferenceError a
wrapUnificationError = first KindInferenceErrorUnification
