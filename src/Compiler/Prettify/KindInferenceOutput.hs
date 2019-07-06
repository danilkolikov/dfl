{- |
Module      :  Compiler.Prettify.KindInferenceOutput
Description :  Prettifying of KindInferenceOutput
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of KindInferenceOutput
-}
module Compiler.Prettify.KindInferenceOutput where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Compiler.Prettify.Utils
import Frontend.Inference.DependencyResolver
import Frontend.Inference.Kind.Ast
import Frontend.Inference.Processor
import Frontend.Inference.Substitution
import Frontend.Syntax.Position (WithLocation(WithLocation))

prettifyKindInferenceOutput :: KindInferenceOutput -> String
prettifyKindInferenceOutput KindInferenceOutput { getKindInferenceOutputState = state
                                                , getKindInferenceOutputDependencies = dependencies
                                                , getKindInferenceOutputDependencyGroups = groups
                                                } =
    unlines $
    [ prettifyHeader "State"
    , prettifyState state
    , prettifyHeader "Dependency graph"
    , prettifyDependencyGraph dependencies
    , prettifyHeader "Group outputs:"
    ] ++
    map prettifyGroupOutput groups

prettifyState :: KindInferenceState -> String
prettifyState KindInferenceState { getResolvedTypeSynonyms = typeSynonyms
                                 , getResolvedClasses = classes
                                 , getResolvedDataTypes = dataTypes
                                 } =
    unlines $
    (prettifyHeader "Type synonyms:" :
     map prettifyTypeSynonym (HM.elems typeSynonyms)) ++
    (prettifyHeader "Data types:" : map prettifyDataType (HM.elems dataTypes)) ++
    (prettifyHeader "Classes:" : map prettifyClass (HM.elems classes))

prettifyTypeSynonym :: TypeSynonym -> String
prettifyTypeSynonym (TypeSynonym name params _) =
    unwords
        [ withKind prettifyIdent name
        , unwords $ map (withKind prettifyIdent) params
        ]

prettifyDataType :: DataType -> String
prettifyDataType (DataType _ name params _ _ _) =
    unwords
        [ withKind prettifyIdent name
        , unwords $ map (withKind prettifyIdent) params
        ]

prettifyClass :: Class -> String
prettifyClass (Class _ (WithLocation name _) param _) =
    unwords [prettifyIdent name, withKind prettifyIdent param]

prettifyDependencyGraph :: DependencyGraph -> String
prettifyDependencyGraph =
    let prettifyNode (node, edges) =
            prettifyIdent node ++
            ": " ++ unwords (map prettifyIdent (HS.toList edges))
     in unlines . map prettifyNode . HM.toList

prettifyGroupOutput :: KindInferenceGroupOutput -> String
prettifyGroupOutput KindInferenceGroupOutput { getKindInferenceGroupOutputIdents = group
                                             , getKindInferenceGroupOutputEqualities = equalities
                                             , getKindInferenceGroupOutputSolution = solution
                                             } =
    unlines $
    [ prettifyHeader $ "Group " ++ unwords (map prettifyIdent group) ++ ":"
    , prettifyHeader "Equalities"
    ] ++
    map prettifyEquality equalities ++
    [prettifyHeader "Solution", prettifySolution solution]

prettifyEquality :: (Kind, Kind) -> String
prettifyEquality (first, second) =
    prettifyKind first ++ " = " ++ prettifyKind second

prettifySolution :: Substitution Kind -> String
prettifySolution =
    let prettifySingle (ident, kind) =
            prettifyIdent ident ++ "::=" ++ prettifyKind kind
     in unlines . map prettifySingle . HM.toList

withKind :: (a -> String) -> WithKind a -> String
withKind prettify (WithKind x _ kind) =
    "(" ++ prettify x ++ ":::" ++ prettifyKind kind ++ ")"

prettifyKind :: Kind -> String
prettifyKind kind =
    case kind of
        KindStar -> "*"
        KindVar name -> prettifyIdent name
        KindFunction from to ->
            "(" ++ prettifyKind from ++ "->" ++ prettifyKind to ++ ")"
