{- |
Module      :  Compiler.Prettify.InferenceDebugOutput
Description :  Prettifying of InferenceDebugOutput
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of InferenceDebugOutput
-}
module Compiler.Prettify.InferenceDebugOutput where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List (intercalate)
import Data.Maybe (catMaybes)

import Compiler.Prettify.Utils
import Frontend.Desugaring.Final.Ast
import Frontend.Inference.Base.DebugOutput
import Frontend.Inference.Base.Variables
import Frontend.Inference.DependencyResolver
import Frontend.Inference.Equalities
import Frontend.Inference.Solver
import Frontend.Inference.Substitution

prettifyInferenceDebugOutput :: InferenceDebugOutput -> String
prettifyInferenceDebugOutput InferenceDebugOutput { getInferenceDebugOutputSignatures = signatures
                                                  , getInferenceDebugOutputDependencyGraph = graph
                                                  , getInferenceDebugOutputDependencyGroups = groups
                                                  , getInferenceDebugOutputDependencyGroupOutputs = outputs
                                                  , getInferenceDebugOutputTypeVariableEqualities = typeVariableEqualities
                                                  } =
    let prettifyDOSignatures sigs =
            unlines
                [ prettifyHeader "Inference of explicit signatures:"
                , prettifySingleGroupDebugOutput sigs
                ]
        prettifyGraph deps =
            unlines
                [ prettifyHeader "Dependency graph"
                , prettifyDependencyGraph deps
                ]
        prettifyGroups grps =
            unlines $
            prettifyHeader "Dependency groups" : map prettifyGroupIdents grps
        prettifyOutputs outs =
            unlines $
            prettifyHeader "Group outputs:" :
            map prettifySingleGroupDebugOutput outs
        prettifyDOTypeVariableEqualities eqs =
            unlines
                [ prettifyHeader "Type variable equalities"
                , prettifyTypeVariableEqualitiesMap eqs
                ]
     in unlines . catMaybes $
        [ prettifyDOSignatures <$> signatures
        , prettifyGraph <$> graph
        , prettifyGroups <$> groups
        , prettifyOutputs <$> outputs
        , prettifyDOTypeVariableEqualities <$> typeVariableEqualities
        ]

prettifyDependencyGraph :: DependencyGraph -> String
prettifyDependencyGraph =
    let prettifyNode (node, edges) =
            prettify node ++ ": " ++ unwords (map prettify (HS.toList edges))
     in unlines . map prettifyNode . HM.toList

prettifyGroupIdents :: HS.HashSet Ident -> String
prettifyGroupIdents = unwords . map prettify . HS.toList

prettifySingleGroupDebugOutput :: SingleGroupInferenceDebugOutput -> String
prettifySingleGroupDebugOutput SingleGroupInferenceDebugOutput { getSingleGroupInferenceDebugOutputGroup = group
                                                               , getSingleGroupInferenceDebugOutputNested = nested
                                                               , getSingleGroupInferenceDebugOutputSolver = solverDebugOutput
                                                               } =
    let prettifyGroupHeader grp =
            prettifyHeader $ "Group: " ++ intercalate ", " (map prettify grp)
        prettifyNested nst =
            unlines
                (prettifyHeader "Nested expressions:" :
                 map prettifyInferenceDebugOutput nst)
     in unlines . catMaybes $
        [ prettifyGroupHeader <$> group
        , prettifyNested <$> nested
        , prettifySolverDebugOutput <$> solverDebugOutput
        ]

prettifySolverDebugOutput :: SolverDebugOutput -> String
prettifySolverDebugOutput SolverDebugOutput { getSolverDebugOutputEqualities = equalities
                                            , getSolverDebugOutputTypeSubstitution = typeSubstitution
                                            , getSolverDebugOutputKindSubstitution = kindSubstitution
                                            , getSolverDebugOutputSortSubstitution = sortSubstitution
                                            , getSolverDebugOutputKindOfTypeVariables = kindOfTypeVariables
                                            , getSolverDebugOutputSortOfKindVariables = sortOfKindVariables
                                            } =
    let prettifyGroupEqualities eqs =
            unlines [prettifyHeader "Equalities", prettifyEqualities eqs]
        prettifySomeSubstitution header sub =
            unlines [prettifyHeader header, prettifySubstitution sub]
     in unlines . catMaybes $
        [ prettifyGroupEqualities <$> equalities
        , prettifySomeSubstitution "Type substitution" <$> typeSubstitution
        , prettifySomeSubstitution "Kind substitution" <$> kindSubstitution
        , prettifySomeSubstitution "Sort substitution" <$> sortSubstitution
        , prettifySomeSubstitution "Kinds of type variables" <$>
          kindOfTypeVariables
        , prettifySomeSubstitution "Sorts of kind variables" <$>
          sortOfKindVariables
        ]

prettifyEqualities :: Equalities -> String
prettifyEqualities Equalities { getTypeEqualities = typeEqualities
                              , getKindEqualities = kindEqualities
                              , getSortEqualities = sortEqualities
                              , getHasKindEqualities = hasKindEqualities
                              , getHasSortEqualities = hasSortEqualities
                              } =
    unlines $
    [prettifyHeader "Type equalities"] ++
    map prettifyEquality typeEqualities ++
    [prettifyHeader "Kind equalities"] ++
    map prettifyEquality kindEqualities ++
    [prettifyHeader "Sort equalities"] ++
    map prettifyEquality sortEqualities ++
    [prettifyHeader "Has kind"] ++
    map prettifyEquality hasKindEqualities ++
    [prettifyHeader "Has sort"] ++ map prettifyEquality hasSortEqualities

prettifyEquality :: (Prettifiable a, Prettifiable b) => (a, b) -> String
prettifyEquality (first, second) = prettify first ++ " = " ++ prettify second

prettifySubstitution :: (Prettifiable a) => Substitution a -> String
prettifySubstitution =
    let prettifySingle (ident, object) =
            prettify ident ++ "::=" ++ prettify object
     in unlines . map prettifySingle . HM.toList

prettifyTypeVariableEqualitiesMap :: TypeVariableEqualitiesMap -> String
prettifyTypeVariableEqualitiesMap =
    let prettifySingle (name, equalities) =
            unlines
                [ prettifyHeader $ "Type variable: " ++ prettify name
                , prettifyTypeVariableEqualities equalities
                ]
     in unlines . map prettifySingle . HM.toList

prettifyTypeVariableEqualities :: TypeVariableEqualities -> String
prettifyTypeVariableEqualities TypeVariableEqualities { getTypeVariableEqualitiesTypes = types
                                                      , getTypeVariableEqualitiesKinds = kinds
                                                      , getTypeVariableEqualitiesSorts = sorts
                                                      } =
    let prettifyLine :: (Prettifiable a) => [a] -> String
        prettifyLine = intercalate " = " . map prettify
     in unlines
            [ "Types: " ++ prettifyLine types
            , "Kinds: " ++ prettifyLine kinds
            , "Sorts: " ++ prettifyLine sorts
            ]
