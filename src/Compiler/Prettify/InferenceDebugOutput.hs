{- |
Module      :  Compiler.Prettify.InferenceDebugOutput
Description :  Prettifying of InferenceDebugOutput
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of InferenceDebugOutput
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.InferenceDebugOutput where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe (catMaybes)

import Compiler.Prettify.Utils
import Frontend.Inference.Equalities
import Frontend.Inference.InferenceProcessor
import Frontend.Inference.Solver
import Frontend.Inference.Substitution
import Util.DependencyResolver

instance (Prettifiable a, Prettifiable s) =>
         Prettifiable (InferenceDebugOutput a s) where
    prettify = prettifyInferenceDebugOutput

instance (Prettifiable a, Prettifiable s) =>
         Prettifiable (SingleGroupInferenceDebugOutput a s) where
    prettify = prettifySingleGroupDebugOutput

prettifyInferenceDebugOutput ::
       (Prettifiable a, Prettifiable s) => InferenceDebugOutput a s -> String
prettifyInferenceDebugOutput InferenceDebugOutput { getInferenceDebugOutputInput = input
                                                  , getInferenceDebugOutputDependencyGraph = graph
                                                  , getInferenceDebugOutputDependencyGroups = groups
                                                  , getInferenceDebugOutputDependencyGroupOutputs = outputs
                                                  , getInferenceDebugOutputSignatures = signatures
                                                  } =
    let prettifyInput i =
            unlines [prettifyHeader "Inference input:", prettify i]
        prettifyOutputs outs =
            unlines $
            prettifyHeader "Group outputs:" :
            map prettifySingleGroupDebugOutput outs
        prettifyDOSignatures sigs =
            unlines [prettifyHeader "Inferred signatures:", prettify sigs]
     in unlines . catMaybes $
        [ prettifyInput <$> input
        , prettifyGraph <$> graph
        , prettifyGroups <$> groups
        , prettifyOutputs <$> outputs
        , prettifyDOSignatures <$> signatures
        ]

prettifyGraph :: (Prettifiable a) => DependencyGraph a -> String
prettifyGraph deps =
    unlines [prettifyHeader "Dependency graph", prettifyDependencyGraph deps]

prettifyGroups :: (Prettifiable a) => [HS.HashSet a] -> String
prettifyGroups grps =
    unlines $ prettifyHeader "Dependency groups" : map prettifyGroupIdents grps

prettifyDependencyGraph :: (Prettifiable a) => DependencyGraph a -> String
prettifyDependencyGraph =
    let prettifyNode (node, edges) =
            prettify node ++ ": " ++ unwords (map prettify (HS.toList edges))
     in unlines . map prettifyNode . HM.toList

prettifyGroupIdents :: Prettifiable a => HS.HashSet a -> String
prettifyGroupIdents = unwords . map prettify . HS.toList

prettifySingleGroupDebugOutput ::
       (Prettifiable a, Prettifiable s)
    => SingleGroupInferenceDebugOutput a s
    -> String
prettifySingleGroupDebugOutput SingleGroupInferenceDebugOutput { getSingleGroupInferenceDebugOutputInput = input
                                                               , getSingleGroupInferenceDebugOutputSolver = solverDebugOutput
                                                               , getSingleGroupInferenceDebugOutputSignatures = signatures
                                                               } =
    let prettifyInput i =
            unlines [prettifyHeader "Inference input:", prettify i]
        prettifyDOSignatures sigs =
            unlines [prettifyHeader "Inferred signatures:", prettify sigs]
     in unlines . catMaybes $
        [ prettifyInput <$> input
        , prettifySolverDebugOutput <$> solverDebugOutput
        , prettifyDOSignatures <$> signatures
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
