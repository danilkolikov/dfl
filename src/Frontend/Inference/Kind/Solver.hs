{- |
Module      :  Frontend.Inference.Kind.Solver
Description :  The solver of kind and sort equalities
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

The solver of kind and sort equalities
-}
module Frontend.Inference.Kind.Solver where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, except, runExcept)
import Control.Monad.Trans.State.Lazy (StateT, modify, runStateT)
import Data.Bifunctor (first)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Frontend.Desugaring.Final.Ast (Ident)
import Frontend.Inference.Kind.ProcessorBase
import Frontend.Inference.Signature
import Frontend.Inference.Substitution
import Frontend.Inference.Unification

-- | A type of debug output of the equality solver
data SolverDebugOutput = SolverDebugOutput
    { getSolverDebugOutputEqualities :: Maybe Equalities -- ^ Equalities between kinds
    , getSolverDebugOutputKindSubstitution :: Maybe (Substitution Kind) -- ^ Solution for kind equalities
    , getSolverDebugOutputSortSubstitution :: Maybe (Substitution Sort) -- ^ Solution for sort equalities
    } deriving (Eq, Show)

-- | Solves the provided system of equalities
solveEqualities ::
       Equalities
    -> Either UnificationError ( (Substitution Kind, Substitution Sort)
                               , SolverDebugOutput)
solveEqualities equalities =
    let emptyOutput =
            SolverDebugOutput
                { getSolverDebugOutputEqualities = Nothing
                , getSolverDebugOutputKindSubstitution = Nothing
                , getSolverDebugOutputSortSubstitution = Nothing
                }
     in runExcept $ runStateT (solveEqualities' equalities) emptyOutput

-- | Applies the solution of a system of equalities to the provided object
applySolution ::
       (KindSubstitutable a, SortSubstitutable a)
    => Equalities
    -> (Substitution Kind, Substitution Sort)
    -> a
    -> a
applySolution equalities (kindSubstitution, sortSubstitution) =
    let substitutedHasSort =
            map
                (first $ substitute kindSubstitution)
                (getHasSortEqualities equalities)
        sortOfVariables = findSortOfVariables substitutedHasSort
     in substituteSort sortSubstitution .
        substituteKind kindSubstitution sortOfVariables

-- | Solves the provided system of equalities and applies solution to the provided
-- | object
solveEqualitiesAndApplySolution ::
       (KindSubstitutable a, SortSubstitutable a)
    => Equalities
    -> a
    -> Either UnificationError (a, SolverDebugOutput)
solveEqualitiesAndApplySolution equalities obj = do
    (solution, debug) <- solveEqualities equalities
    return (applySolution equalities solution obj, debug)

-- | A type of the solver of equalities
type Solver = StateT SolverDebugOutput (Except UnificationError)

-- | Solves the provided system of equalities
solveEqualities' :: Equalities -> Solver (Substitution Kind, Substitution Sort)
solveEqualities' equalities@Equalities { getKindEqualities = kindEqualities
                                       , getSortEqualities = sortEqualities
                                       , getHasSortEqualities = hasSortEqualities
                                       } = do
    modify $ \s -> s {getSolverDebugOutputEqualities = Just equalities}
    kindSubstitution <- lift . except $ unifyEqualities kindEqualities
    modify $ \s ->
        s {getSolverDebugOutputKindSubstitution = Just kindSubstitution}
    -- Append extra sort equalities and solve them
    let extraSortEqualities =
            createSortEqualities kindSubstitution hasSortEqualities
        allSortEqualities = sortEqualities ++ extraSortEqualities
    sortSubstitution <- lift . except $ unifyEqualities allSortEqualities
    -- Replace all unbound sort variables with []
    let fixedSortSubstitution =
            fixSortVariables hasSortEqualities sortSubstitution
    modify $ \s ->
        s {getSolverDebugOutputSortSubstitution = Just fixedSortSubstitution}
    return (kindSubstitution, fixedSortSubstitution)

-- | Create additional equalities between sorts using a solution for kind equalities
createSortEqualities :: Substitution Kind -> [(Kind, Sort)] -> [(Sort, Sort)]
createSortEqualities sub hasSortEqualities =
    let substituted = map (first $ substitute sub) hasSortEqualities
        grouped = groupHasSortEqualities substituted
        createEqualities [] = []
        createEqualities [_] = []
        createEqualities (s:rest) = map (\t -> (s, t)) rest
     in concatMap (createEqualities . snd) grouped

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

-- | Find sort of free variables
findSortOfVariables :: [(Kind, Sort)] -> [(Ident, Sort)]
findSortOfVariables =
    let processSingle (KindVar name, s) = [(name, s)]
        processSingle _ = []
     in concatMap processSingle
