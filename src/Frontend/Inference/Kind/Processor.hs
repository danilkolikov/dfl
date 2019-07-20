{- |
Module      :  Frontend.Inference.Kind.Processor
Description :  Functions for kind inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of kind inference
-}
module Frontend.Inference.Kind.Processor where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.State.Lazy
    ( State
    , StateT
    , execStateT
    , get
    , modify
    , put
    , runState
    )
import Data.Bifunctor (first)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Frontend.Desugaring.Final.Ast
import Frontend.Inference.DependencyResolver
import Frontend.Inference.Kind.DependencyGroupResolver
import Frontend.Inference.Kind.Equalities
import Frontend.Inference.Kind.ProcessorBase
import Frontend.Inference.Kind.Signatures
import Frontend.Inference.Kind.WithDependencies
import Frontend.Inference.Signature
import Frontend.Inference.Substitution
import Frontend.Inference.Unification
import Frontend.Inference.Variables

-- | Errors which may be encountered during kind inference
data KindInferenceError
    = KindInferenceErrorDependencyResolution DependencyResolverError -- ^ Error happened during dependency resolution
    | KindInferenceErrorEqualityGeneration EqualityGenerationError -- ^ Error happened during resolution of a single group
    | KindInferenceErrorUnification UnificationError -- ^ Error happened during unification
    deriving (Eq, Show)

-- | A debug output of kind inference
data KindInferenceDebugOutput = KindInferenceDebugOutput
    { getKindInferenceDebugOutputDependencies :: Maybe DependencyGraph -- ^ Dependency graph of a module
    , getKindInferenceDebugOutputDependencyGroups :: Maybe [HS.HashSet Ident] -- ^ Dependency groups
    , getKindInferenceDebugOutputDependencyGroupOutputs :: Maybe [KindInferenceGroupDebugOutput] -- ^ Processed dependency groups
    , getKindInferenceDebugOutputSignatures :: Maybe Signatures -- ^ Inferred signatures
    } deriving (Eq, Show)

-- | A debug output of inference of a single dependency group
data KindInferenceGroupDebugOutput = KindInferenceGroupDebugOutput
    { getKindInferenceGroupDebugOutputIdents :: Maybe [DependencyGroupItemEmpty] -- ^ List of idents in a group
    , getKindInferenceGroupDebugOutputInitialSignatures :: Maybe Signatures -- ^ Initial signatures
    , getKindInferenceGroupDebugOutputEqualities :: Maybe Equalities -- ^ Equalities between kinds
    , getKindInferenceGroupDebugOutputKindSubstitution :: Maybe (Substitution Kind) -- ^ Solution for kind equalities
    , getKindInferenceGroupDebugOutputSortSubstitution :: Maybe (Substitution Sort) -- ^ Solution for sort equalities
    , getKindInferenceGroupDebugOutputSignatures :: Maybe Signatures -- ^ Inferred signatures
    } deriving (Eq, Show)

-- | A type of the processor of kind inference
type KindInferenceProcessor a
     = ReaderT Environment (ExceptT KindInferenceError (State KindInferenceDebugOutput)) a

-- | Write a field to debug output
modifyDebugOutput ::
       (KindInferenceDebugOutput -> KindInferenceDebugOutput)
    -> KindInferenceProcessor ()
modifyDebugOutput = lift . lift . modify

-- | Infer kinds of data types, type synonyms and classes
inferKinds ::
       DataTypes
    -> TypeSynonyms
    -> Classes
    -> Signatures
    -> (Either KindInferenceError Signatures, KindInferenceDebugOutput)
inferKinds dataTypes typeSynonyms classes initialState =
    let environment =
            Environment
                { getDataTypes = dataTypes
                , getTypeSynonyms = typeSynonyms
                , getClasses = classes
                }
        emptyDebugOutput =
            KindInferenceDebugOutput
                { getKindInferenceDebugOutputDependencies = Nothing
                , getKindInferenceDebugOutputDependencyGroups = Nothing
                , getKindInferenceDebugOutputDependencyGroupOutputs = Nothing
                , getKindInferenceDebugOutputSignatures = Nothing
                }
        reader = inferKinds' initialState
        exceptT = runReaderT reader environment
        state = runExceptT exceptT
     in runState state emptyDebugOutput

-- | Infer kinds of data types, type synonyms and classes
inferKinds' :: Signatures -> KindInferenceProcessor Signatures
inferKinds' initialState = do
    env <- ask
    -- Find dependencies between definitions
    let dependencyGraph = getModuleDependencyGraph env
    modifyDebugOutput $ \s ->
        s {getKindInferenceDebugOutputDependencies = Just dependencyGraph}
    -- Find dependency groups in this graph
    let dependencyGroups = getDependencyGroups dependencyGraph
    groups <- lift . except $ wrapDependencyResolverError dependencyGroups
    modifyDebugOutput $ \s ->
        s {getKindInferenceDebugOutputDependencyGroups = Just groups}
    -- Infer kinds in each group in reversed order
    let inferGroups = mapM (inferSingleGroup . HS.toList) (reverse groups)
        state = runReaderT inferGroups env
        exceptT = execStateT state initialState
        debugState = runExceptT exceptT
        (inferredSignatures, debugOutput) = runState debugState []
    -- Save debug output of steps
    modifyDebugOutput $ \s ->
        s {getKindInferenceDebugOutputDependencyGroupOutputs = Just $ reverse debugOutput}
    -- Return found signatures
    signatures <- lift $ except inferredSignatures
    modifyDebugOutput $ \s ->
        s {getKindInferenceDebugOutputSignatures = Just signatures}
    return signatures

-- | Processor of kind inference of a single dependency group
type SingleGroupInferenceProcessor a
     = ReaderT Environment (StateT Signatures (ExceptT KindInferenceError (State [KindInferenceGroupDebugOutput]))) a

-- | Append an empty object to the group debug output
appendEmptyGroupDebugOutput :: SingleGroupInferenceProcessor ()
appendEmptyGroupDebugOutput =
    let emptyDebugOutput =
            KindInferenceGroupDebugOutput
                { getKindInferenceGroupDebugOutputIdents = Nothing
                , getKindInferenceGroupDebugOutputInitialSignatures = Nothing
                , getKindInferenceGroupDebugOutputEqualities = Nothing
                , getKindInferenceGroupDebugOutputKindSubstitution = Nothing
                , getKindInferenceGroupDebugOutputSortSubstitution = Nothing
                , getKindInferenceGroupDebugOutputSignatures = Nothing
                }
     in lift . lift . lift . modify $ \s -> emptyDebugOutput : s

-- | Set a field in the debug output of a group
modifyGroupDebugOutput ::
       (KindInferenceGroupDebugOutput -> KindInferenceGroupDebugOutput)
    -> SingleGroupInferenceProcessor ()
modifyGroupDebugOutput f = lift . lift . lift . modify $ \(s:rest) -> f s : rest

-- | Infer kinds of a single dependency group
inferSingleGroup :: [Ident] -> SingleGroupInferenceProcessor ()
inferSingleGroup group = do
    env <- ask
    existingSignatures <- lift get
    -- Start processing
    appendEmptyGroupDebugOutput
    -- Resolve identifiers in the group
    let resolvedGroup = resolveDependencyGroup group env
    modifyGroupDebugOutput $ \s ->
        s {getKindInferenceGroupDebugOutputIdents = Just resolvedGroup}
    -- Collect equalities in the group
    (signatures, equalities@Equalities { getKindEqualities = kindEqualities
                                       , getSortEqualities = sortEqualities
                                       , getHasSortEqualities = hasSortEqualities
                                       }) <-
        lift . lift . except . wrapEqualityGenerationError $
        createEqualitiesForGroup resolvedGroup existingSignatures
    modifyGroupDebugOutput $ \s ->
        s
            { getKindInferenceGroupDebugOutputInitialSignatures =
                  Just signatures
            , getKindInferenceGroupDebugOutputEqualities = Just equalities
            }
    -- Solve kind equalities
    kindSubstitution <-
        lift . lift . except . wrapUnificationError $
        unifyEqualities kindEqualities
    modifyGroupDebugOutput $ \s ->
        s
            { getKindInferenceGroupDebugOutputKindSubstitution =
                  Just kindSubstitution
            }
    -- Append extra sort equalities and solve them
    let (substitutedHasSortEqualities, extraSortEqualities) =
            createSortEqualities kindSubstitution hasSortEqualities
        sortOfVariables = findSortOfVariables substitutedHasSortEqualities
        allSortEqualities = sortEqualities ++ extraSortEqualities
    sortSubstitution <-
        lift . lift . except . wrapUnificationError $
        unifyEqualities allSortEqualities
    -- Replace all unbound sort variables with []
    let fixedSortSubstitution =
            fixSortVariables hasSortEqualities sortSubstitution
    modifyGroupDebugOutput $ \s ->
        s
            { getKindInferenceGroupDebugOutputSortSubstitution =
                  Just fixedSortSubstitution
            }
    -- Substitute all variables with found values
    let inferredSignatures =
            substituteSort fixedSortSubstitution .
            substituteKind kindSubstitution sortOfVariables $
            signatures
    modifyGroupDebugOutput $ \s ->
        s {getKindInferenceGroupDebugOutputSignatures = Just inferredSignatures}
    -- Add inferred signatures to the state
    let newState = existingSignatures <> inferredSignatures
    lift $ put newState

-- | Create equalities between items in one group
createEqualitiesForGroup ::
       [DependencyGroupItemEmpty]
    -> Signatures
    -> Either EqualityGenerationError (Signatures, Equalities)
createEqualitiesForGroup group signatures =
    evalVariableGenerator $ do
        (extendedGroup, localSignatures) <- createSignaturesForGroup group
        let allSignatures = signatures <> localSignatures
        equalities <- generateEqualitiesForGroup extendedGroup allSignatures
        return $ (\c -> (localSignatures, c)) <$> equalities

-- | Create additional equalities between sorts using a solution for kind equalities
createSortEqualities ::
       Substitution Kind -> [(Kind, Sort)] -> ([(Kind, Sort)], [(Sort, Sort)])
createSortEqualities sub hasSortEqualities =
    let substituted = map (first $ substitute sub) hasSortEqualities
        grouped = groupHasSortEqualities substituted
        createEqualities [] = []
        createEqualities [_] = []
        createEqualities (s:rest) = map (\t -> (s, t)) rest
        equalities = concatMap (createEqualities . snd) grouped
     in (substituted, equalities)

-- | Group equal sorts by corresponding kinds
groupHasSortEqualities :: [(Kind, Sort)] -> [(Kind, [Sort])]
groupHasSortEqualities = foldr insertSortEquality []
  where
    insertSortEquality :: (Kind, Sort) -> [(Kind, [Sort])] -> [(Kind, [Sort])]
    insertSortEquality x@(kind, sort) gr =
        case gr of
            [] -> [(kind, [sort])]
            firstGroup@(grKind, grSorts):rest ->
                if kind == grKind
                    then (grKind, sort : grSorts) : rest
                    else firstGroup : insertSortEquality x rest

-- | Find sort of free variables
findSortOfVariables :: [(Kind, Sort)] -> [(Ident, Sort)]
findSortOfVariables =
    let processSingle (KindVar name, s) = [(name, s)]
        processSingle _ = []
     in concatMap processSingle

-- | Replace free sort variables with []
fixSortVariables :: [(Kind, Sort)] -> Substitution Sort -> Substitution Sort
fixSortVariables hasSortEqualities sub =
    let freeVariables =
            HS.unions . map getFreeVariables $
            HM.elems sub ++ map snd hasSortEqualities
        fixVariablesSubstitution =
            HM.fromList . map (\v -> (v, SortSquare)) . HS.toList $
            freeVariables
     in sub `compose` fixVariablesSubstitution

-- | Wrap dependency resolver error
wrapDependencyResolverError ::
       Either DependencyResolverError a -> Either KindInferenceError a
wrapDependencyResolverError = first KindInferenceErrorDependencyResolution

-- | Wrap dependency group resolving error
wrapEqualityGenerationError ::
       Either EqualityGenerationError a -> Either KindInferenceError a
wrapEqualityGenerationError = first KindInferenceErrorEqualityGeneration

-- | Wrap unification error
wrapUnificationError :: Either UnificationError a -> Either KindInferenceError a
wrapUnificationError = first KindInferenceErrorUnification
