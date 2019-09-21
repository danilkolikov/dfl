{- |
Module      :  Frontend.Inference.Type.Solver
Description :  The solver of type, kind and sort equalities
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

The solver of type, kind and sort equalities
-}
module Frontend.Inference.Solver where

import Control.Applicative ((<|>))
import Data.Bifunctor (first, second)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe, mapMaybe)

import Frontend.Desugaring.Final.Ast (Ident)
import Frontend.Inference.AlgebraicExp
import Frontend.Inference.Equalities
import Frontend.Inference.Signature
import Frontend.Inference.Substitution
import Frontend.Inference.Unification
import Frontend.Inference.Util.Debug
import Frontend.Inference.WithVariables

-- | A solution of a system of type, kind and sort equalities
data Solution = Solution
    { getSolutionTypeSubstitution :: Substitution Type
    , getSolutionKindSubstitution :: Substitution Kind
    , getSolutionSortSubstitution :: Substitution Sort
    , getSolutionKindOfTypeVariables :: Substitution Kind
    , getSolutionSortOfKindVariables :: Substitution Sort
    , getSolutionTypeConstraints :: [Constraint]
    }

-- | A debug output of the equality solver
data SolverDebugOutput = SolverDebugOutput
    { getSolverDebugOutputEqualities :: Maybe Equalities -- ^ Equalities between kinds
    , getSolverDebugOutputTypeSubstitution :: Maybe (Substitution Type) -- ^ Solution for type equalities
    , getSolverDebugOutputKindSubstitution :: Maybe (Substitution Kind) -- ^ Solution for kind equalities
    , getSolverDebugOutputSortSubstitution :: Maybe (Substitution Sort) -- ^ Solution for sort equalities
    , getSolverDebugOutputKindOfTypeVariables :: Maybe (Substitution Kind) -- ^ Kinds of type variables
    , getSolverDebugOutputSortOfKindVariables :: Maybe (Substitution Sort) -- ^ Sort of kind variables
    , getSolverDebugOutputTypeConstraints :: Maybe [Constraint] -- ^ Type constraints
    } deriving (Eq, Show)

instance Semigroup SolverDebugOutput where
    SolverDebugOutput e1 t1 k1 s1 kt1 sk1 tc1 <> SolverDebugOutput e2 t2 k2 s2 kt2 sk2 tc2 =
        SolverDebugOutput
            (e1 <|> e2)
            (t1 <|> t2)
            (k1 <|> k2)
            (s1 <|> s2)
            (kt1 <|> kt2)
            (sk1 <|> sk2)
            (tc1 <|> tc2)

instance Monoid SolverDebugOutput where
    mempty = SolverDebugOutput mempty mempty mempty mempty mempty mempty mempty

-- | Solves the provided system of equalities
solveEqualities ::
       Equalities -> (Either UnificationError Solution, SolverDebugOutput)
solveEqualities = runWithDebugOutput . solveEqualities'

-- | Applies the solution of a system to the object, supporting substitution of sorts
applySortSolution :: (SortSubstitutable a) => Solution -> a -> a
applySortSolution Solution {getSolutionSortSubstitution = sortSubstitution} =
    substituteSort sortSubstitution

-- | Applies the solution of a system to the object, supporting substitution of sorts and kinds
applyKindSolution ::
       (SortSubstitutable a, KindSubstitutable a) => Solution -> a -> a
applyKindSolution solution@Solution {getSolutionKindSubstitution = kindSubstitution} =
    applySortSolution solution . substituteKind kindSubstitution

-- | Applies the solution of a system to the object, supporting substitution of sorts, kinds and types
applyTypeSolution ::
       (SortSubstitutable a, KindSubstitutable a, TypeSubstitutable a)
    => Solution
    -> a
    -> a
applyTypeSolution solution@Solution {getSolutionTypeSubstitution = typeSubstitution} =
    applyKindSolution solution . substituteType typeSubstitution

-- | Applies the solution of a system to the object, supporting substitution of sorts and kinds,
-- | and generalises the result
applyKindSolutionAndGeneralise ::
       (SortSubstitutable a, KindSubstitutable a, KindGeneralisable a)
    => Solution
    -> a
    -> a
applyKindSolutionAndGeneralise solution
    | Solution { getSolutionKindSubstitution = kindSubstitution
               , getSolutionSortOfKindVariables = sortOfKindVariables
               } <- solution =
        applySortSolution solution .
        generaliseKind sortOfKindVariables . substituteKind kindSubstitution

-- | Applies the solution of a system to the object, supporting substitution of sorts, kinds and types,
-- | and generalises the result
applyTypeSolutionAndGeneralise ::
       ( SortSubstitutable a
       , KindSubstitutable a
       , KindGeneralisable a
       , TypeSubstitutable a
       , TypeGeneralisable a
       )
    => Solution
    -> a
    -> a
applyTypeSolutionAndGeneralise solution
    | Solution { getSolutionTypeSubstitution = typeSubstitution
               , getSolutionKindOfTypeVariables = kindOfTypeVariables
               } <- solution =
        applyKindSolutionAndGeneralise solution .
        generaliseType kindOfTypeVariables . substituteType typeSubstitution

-- | Applies kind solutions and sets kinds of free type variables
applyKindSolutionAndSetTypeVariables ::
       [Ident]
    -> Solution
    -> TypeConstructorSignature
    -> TypeConstructorSignature
applyKindSolutionAndSetTypeVariables vars sol signature =
    let kindApplied = applyKindSolutionAndGeneralise sol signature
        combinedKind = getTypeConstructorSignatureKind kindApplied
        cutVars [] kind = ([], kind)
        cutVars (name:rest) kind =
            case kind of
                KindFunction from to ->
                    let (resVars, resKind) = cutVars rest to
                     in ((name, from) : resVars, resKind)
                _ -> error "Unexpected kind"
        (typeVariables, resultKind) = cutVars vars combinedKind
     in kindApplied
            { getTypeConstructorSignatureKind = resultKind
            , getTypeConstructorSignatureTypeParams = typeVariables
            }

-- | Selects such type constraints, which bound at least one type variable,
-- | used in signature
getRelevantTypeConstraints ::
       (WithTypeParams a) => Solution -> a -> [Constraint]
getRelevantTypeConstraints Solution {getSolutionTypeConstraints = constraints} sig =
    let typeParams = HS.fromList . map fst $ getTypeParams sig
        isRelevant constr =
            not . null $ getFreeVariables constr `HS.intersection` typeParams
     in filter isRelevant constraints

-- | A type of the solver of equalities
type Solver = WithDebugOutput UnificationError SolverDebugOutput

-- | Unifies the system of equalities
unifyEqualitiesSystem ::
       (IsAlgebraicExp a) => [(a, a)] -> Solver (Substitution a)
unifyEqualitiesSystem = wrapEither id . unifyEqualities

-- | Solves the provided system of equalities
solveEqualities' :: Equalities -> Solver Solution
solveEqualities' equalities@Equalities { getTypeEqualities = typeEqualities
                                       , getKindEqualities = kindEqualities
                                       , getSortEqualities = sortEqualities
                                       , getHasKindEqualities = hasKindEqualities
                                       , getHasSortEqualities = hasSortEqualities
                                       , getTypeConstraints = typeConstraints
                                       } = do
    writeDebugOutput mempty {getSolverDebugOutputEqualities = Just equalities}
    -- Solve type equalities
    typeSubstitution <- unifyEqualitiesSystem typeEqualities
    writeDebugOutput
        mempty {getSolverDebugOutputTypeSubstitution = Just typeSubstitution}
    -- Append extra kind equalities and solve them
    let extraKindEqualities =
            createExtraEqualities typeSubstitution hasKindEqualities
        allKindEqualities = kindEqualities ++ extraKindEqualities
    kindSubstitution <- unifyEqualitiesSystem allKindEqualities
    writeDebugOutput
        mempty {getSolverDebugOutputKindSubstitution = Just kindSubstitution}
    -- Append extra sort equalities and solve them
    let extraSortEqualities =
            createExtraEqualities kindSubstitution hasSortEqualities
        allSortEqualities = sortEqualities ++ extraSortEqualities
    sortSubstitution <- unifyEqualitiesSystem allSortEqualities
    -- Replace all unbound sort variables with []
    let fixedSortSubstitution =
            fixSortVariables hasSortEqualities sortSubstitution
    writeDebugOutput
        mempty
            {getSolverDebugOutputSortSubstitution = Just fixedSortSubstitution}
    let makeMapping sub =
            HM.fromList . map (second $ substitute sub) . findVariables
        kindOfTypeVariables = makeMapping kindSubstitution hasKindEqualities
        sortOfKindVariables =
            makeMapping fixedSortSubstitution hasSortEqualities
        finalConstraints =
            getConstraintsWithVariables typeConstraints typeSubstitution
    writeDebugOutput
        mempty
            { getSolverDebugOutputKindOfTypeVariables = Just kindOfTypeVariables
            , getSolverDebugOutputSortOfKindVariables = Just sortOfKindVariables
            , getSolverDebugOutputTypeConstraints = Just finalConstraints
            }
    return
        Solution
            { getSolutionTypeSubstitution = typeSubstitution
            , getSolutionKindSubstitution = kindSubstitution
            , getSolutionSortSubstitution = fixedSortSubstitution
            , getSolutionKindOfTypeVariables = kindOfTypeVariables
            , getSolutionSortOfKindVariables = sortOfKindVariables
            , getSolutionTypeConstraints = finalConstraints
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

-- | Selects constraints which have only type variables
getConstraintsWithVariables :: [Constraint] -> Substitution Type -> [Constraint]
getConstraintsWithVariables constraints sub =
    let substituted = map (substituteType sub) constraints
        isVariable (TypeVar _) = True
        isVariable _ = False
        hasOnlyVariables constr =
            case constr of
                ConstraintVariable _ t -> isVariable t
                ConstraintAppliedVariable _ t _ -> isVariable t
     in filter hasOnlyVariables substituted
