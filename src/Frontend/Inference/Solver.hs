{- |
Module      :  Frontend.Inference.Type.Solver
Description :  The solver of type, kind and sort equalities
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

The solver of type, kind and sort equalities
-}
module Frontend.Inference.Solver where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, except, runExcept)
import Control.Monad.Trans.State.Lazy (StateT, modify, runStateT)
import Data.Bifunctor (first)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe, mapMaybe)

import Frontend.Desugaring.Final.Ast (Ident)
import Frontend.Inference.Equalities
import Frontend.Inference.Expression
import Frontend.Inference.Signature
import Frontend.Inference.Substitution
import Frontend.Inference.Unification

-- | A solution of a system of type, kind and sort equalities
data Solution = Solution
    { getSolutionTypeSubstitution :: Substitution Type
    , getSolutionKindSubstitution :: Substitution Kind
    , getSolutionSortSubstitution :: Substitution Sort
    }

-- | A debug output of the equality solver
data SolverDebugOutput = SolverDebugOutput
    { getSolverDebugOutputEqualities :: Maybe Equalities -- ^ Equalities between kinds
    , getSolverDebugOutputTypeSubstitution :: Maybe (Substitution Type) -- ^ Solution for type equalities
    , getSolverDebugOutputKindSubstitution :: Maybe (Substitution Kind) -- ^ Solution for kind equalities
    , getSolverDebugOutputSortSubstitution :: Maybe (Substitution Sort) -- ^ Solution for sort equalities
    } deriving (Eq, Show)

-- | Solves the provided system of equalities
solveEqualities ::
       Equalities -> Either UnificationError (Solution, SolverDebugOutput)
solveEqualities equalities =
    let emptyOutput =
            SolverDebugOutput
                { getSolverDebugOutputEqualities = Nothing
                , getSolverDebugOutputTypeSubstitution = Nothing
                , getSolverDebugOutputKindSubstitution = Nothing
                , getSolverDebugOutputSortSubstitution = Nothing
                }
     in runExcept $ runStateT (solveEqualities' equalities) emptyOutput

-- | Applies the solution of a system to the object, supporting substitution of sorts
applySortSolution :: (SortSubstitutable a) => Equalities -> Solution -> a -> a
applySortSolution _ Solution {getSolutionSortSubstitution = sortSubstitution} =
    substituteSort sortSubstitution

-- | Applies the solution of a system to the object, supporting substitution of sorts and kinds
applyKindSolution ::
       (SortSubstitutable a, KindSubstitutable a)
    => Equalities
    -> Solution
    -> a
    -> a
applyKindSolution equalities solution@Solution {getSolutionKindSubstitution = kindSubstitution} =
    let substitutedHasSort =
            map
                (first $ substitute kindSubstitution)
                (getHasSortEqualities equalities)
        sortOfVariables = findVariables substitutedHasSort
     in applySortSolution equalities solution .
        substituteKind kindSubstitution sortOfVariables


-- | Applies the solution of a system to the object, supporting substitution of sorts, kinds and types
applyTypeSolution ::
       (TypeSubstitutable a, KindSubstitutable a, SortSubstitutable a)
    => Equalities
    -> Solution
    -> a
    -> a
applyTypeSolution equalities solution@Solution {getSolutionTypeSubstitution = typeSubstitution} =
    let substitutedHasKind =
            map
                (first $ substitute typeSubstitution)
                (getHasKindEqualities equalities)
        kindOfVariables = findVariables substitutedHasKind
     in applyKindSolution equalities solution .
        substituteType typeSubstitution kindOfVariables

-- | Solves the provided system of equalities and applies solution to the provided
-- | object
solveEqualitiesAndApplySolution ::
       (TypeSubstitutable a, KindSubstitutable a, SortSubstitutable a)
    => Equalities
    -> a
    -> Either UnificationError (a, SolverDebugOutput)
solveEqualitiesAndApplySolution equalities obj = do
    (solution, debug) <- solveEqualities equalities
    return (applyTypeSolution equalities solution obj, debug)

-- | A type of the solver of equalities
type Solver = StateT SolverDebugOutput (Except UnificationError)

-- | Solves the provided system of equalities
solveEqualities' :: Equalities -> Solver Solution
solveEqualities' equalities@Equalities { getTypeEqualities = typeEqualities
                                       , getKindEqualities = kindEqualities
                                       , getSortEqualities = sortEqualities
                                       , getHasKindEqualities = hasKindEqualities
                                       , getHasSortEqualities = hasSortEqualities
                                       } = do
    modify $ \s -> s {getSolverDebugOutputEqualities = Just equalities}
    -- Solve type equalities
    typeSubstitution <- lift . except $ unifyEqualities typeEqualities
    modify $ \s ->
        s {getSolverDebugOutputTypeSubstitution = Just typeSubstitution}
    -- Append extra kind equalities and solve them
    let extraKindEqualities =
            createExtraEqualities typeSubstitution hasKindEqualities
        allKindEqualities = kindEqualities ++ extraKindEqualities
    kindSubstitution <- lift . except $ unifyEqualities allKindEqualities
    modify $ \s ->
        s {getSolverDebugOutputKindSubstitution = Just kindSubstitution}
    -- Append extra sort equalities and solve them
    let extraSortEqualities =
            createExtraEqualities kindSubstitution hasSortEqualities
        allSortEqualities = sortEqualities ++ extraSortEqualities
    sortSubstitution <- lift . except $ unifyEqualities allSortEqualities
    -- Replace all unbound sort variables with []
    let fixedSortSubstitution =
            fixSortVariables hasSortEqualities sortSubstitution
    modify $ \s ->
        s {getSolverDebugOutputSortSubstitution = Just fixedSortSubstitution}
    return
        Solution
            { getSolutionTypeSubstitution = typeSubstitution
            , getSolutionKindSubstitution = kindSubstitution
            , getSolutionSortSubstitution = fixedSortSubstitution
            }

-- | Create additional equalities between sorts using a solution for kind equalities
createExtraEqualities ::
       (Substitutable a, WithVariables a, Eq a)
    => Substitution a
    -> [(a, b)]
    -> [(b, b)]
createExtraEqualities sub typingEqualities =
    let substituted = map (first $ substitute sub) typingEqualities
        vars = findVariables substituted
        grouped = HM.toList $ groupTypingEqualities vars
        createEqualities [] = []
        createEqualities [_] = []
        createEqualities (s:rest) = map (\t -> (s, t)) rest
     in concatMap (createEqualities . snd) grouped

-- | Group equal sorts by corresponding kinds
groupTypingEqualities :: [(Ident, b)] -> HM.HashMap Ident [b]
groupTypingEqualities = foldr insertTypingEquality HM.empty
  where
    insertTypingEquality (key, val) res =
        let group = fromMaybe [] (HM.lookup key res)
         in HM.insert key (val : group) res

-- | Finds mapped variables
findVariables :: (WithVariables a) => [(a, b)] -> [(Ident, b)]
findVariables = mapMaybe mapVariable
  where
    mapVariable (key, val) = (\x -> (x, val)) <$> getVariableName key

-- | Replaces all undbound sort variables with []
fixSortVariables :: [(Kind, Sort)] -> Substitution Sort -> Substitution Sort
fixSortVariables hasSort sub =
    let sorts = map (substitute sub . snd) hasSort
        freeVariables = HS.unions . map getFreeVariables $ sorts ++ HM.elems sub
        fixedVariables =
            HM.fromList . map (\x -> (x, SortSquare)) . HS.toList $
            freeVariables
     in sub `compose` fixedVariables
