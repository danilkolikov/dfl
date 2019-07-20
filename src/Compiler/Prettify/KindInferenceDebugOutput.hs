{- |
Module      :  Compiler.Prettify.KindInferenceDebugOutput
Description :  Prettifying of KindInferenceDebugOutput
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of KindInferenceDebugOutput
-}
module Compiler.Prettify.KindInferenceDebugOutput where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List (intercalate)
import Data.Maybe (catMaybes)

import Compiler.Prettify.Utils
import Frontend.Desugaring.Final.Ast
import Frontend.Inference.DependencyResolver
import Frontend.Inference.Processor
import Frontend.Inference.Signature
import Frontend.Inference.Substitution
import Frontend.Syntax.Position (WithLocation(getValue))

prettifyKindInferenceDebugOutput :: KindInferenceDebugOutput -> String
prettifyKindInferenceDebugOutput KindInferenceDebugOutput { getKindInferenceDebugOutputDependencies = dependencies
                                                          , getKindInferenceDebugOutputDependencyGroups = groups
                                                          , getKindInferenceDebugOutputDependencyGroupOutputs = outputs
                                                          , getKindInferenceDebugOutputSignatures = signatures
                                                          } =
    let prettifyGraph deps =
            unlines
                [ prettifyHeader "Dependency graph"
                , prettifyDependencyGraph deps
                ]
        prettifyGroups grps =
            unlines $
            prettifyHeader "Dependency groups" : map prettifyGroupIdents grps
        prettifyOutputs outs =
            unlines $
            prettifyHeader "Group outputs:" : map prettifyGroupDebugOutput outs
        prettifySignaturesWithHeader sigs =
            unlines [prettifyHeader "Signatures", prettifySignatures sigs]
     in unlines . catMaybes $
        [ prettifyGraph <$> dependencies
        , prettifyGroups <$> groups
        , prettifyOutputs <$> outputs
        , prettifySignaturesWithHeader <$> signatures
        ]

prettifySignatures :: Signatures -> String
prettifySignatures Signatures { getTypeSynonymSignatures = typeSynonyms
                              , getDataTypeSignatures = classes
                              , getClassSignatures = dataTypes
                              } =
    unlines $
    (prettifyHeader "Type synonyms:" :
     map prettifySignature (HM.toList typeSynonyms)) ++
    (prettifyHeader "Data types:" : map prettifySignature (HM.toList classes)) ++
    (prettifyHeader "Classes:" : map prettifySignature (HM.toList dataTypes))

prettifySignature :: (Ident, TypeConstructorSignature) -> String
prettifySignature (name, sig@TypeConstructorSignature { getTypeConstructorSignatureKindParams = kindParams
                                                      , getTypeConstructorSignatureTypeParams = typeParams
                                                      }) =
    unwords
        [ prettifyIdent name
        , "::"
        , prettifyForAll typeParams
        , "::"
        , prettifyForAll kindParams ++ prettifyKind (getFullKind sig)
        , "::"
        , prettifySort (getFullSort sig)
        ]

prettifyDependencyGraph :: DependencyGraph -> String
prettifyDependencyGraph =
    let prettifyNode (node, edges) =
            prettifyIdent node ++
            ": " ++ unwords (map prettifyIdent (HS.toList edges))
     in unlines . map prettifyNode . HM.toList

prettifyGroupIdents :: HS.HashSet Ident -> String
prettifyGroupIdents = unwords . map prettifyIdent . HS.toList

prettifyGroupDebugOutput :: KindInferenceGroupDebugOutput -> String
prettifyGroupDebugOutput KindInferenceGroupDebugOutput { getKindInferenceGroupDebugOutputIdents = group
                                                       , getKindInferenceGroupDebugOutputInitialSignatures = initialSignatures
                                                       , getKindInferenceGroupDebugOutputEqualities = equalities
                                                       , getKindInferenceGroupDebugOutputKindSubstitution = kindSubstitution
                                                       , getKindInferenceGroupDebugOutputSortSubstitution = sortSubstitution
                                                       , getKindInferenceGroupDebugOutputSignatures = signatures
                                                       } =
    let prettifyGroupHeader grp =
            prettifyHeader $
            "Group: " ++ intercalate ", " (map prettifyGroup grp)
        prettifyInitialSignatures sigs =
            unlines
                [prettifyHeader "Initial signatures", prettifySignatures sigs]
        prettifyGroupEqualities eqs =
            unlines [prettifyHeader "Equalities", prettifyEqualities eqs]
        prettifyKindSubstitution sub =
            unlines
                [ prettifyHeader "Kind substitution"
                , prettifySubstitution prettifyKind sub
                ]
        prettifySortSubstitution sub =
            unlines
                [ prettifyHeader "Sort substitution"
                , prettifySubstitution prettifySort sub
                ]
        prettifyGroupSignatures sigs =
            unlines
                [prettifyHeader "Inferred signatures", prettifySignatures sigs]
     in unlines . catMaybes $
        [ prettifyGroupHeader <$> group
        , prettifyInitialSignatures <$> initialSignatures
        , prettifyGroupEqualities <$> equalities
        , prettifyKindSubstitution <$> kindSubstitution
        , prettifySortSubstitution <$> sortSubstitution
        , prettifyGroupSignatures <$> signatures
        ]

prettifyGroup :: DependencyGroupItemEmpty -> String
prettifyGroup item =
    case item of
        DependencyGroupItemTypeSynonym TypeSynonym {getTypeSynonymName = name} _ ->
            "Type synonym " ++ prettifyIdent (getValue name)
        DependencyGroupItemDataType DataType {getDataTypeName = name} _ ->
            "Data type " ++ prettifyIdent (getValue name)
        DependencyGroupItemClass Class {getClassName = name} _ ->
            "Class " ++ prettifyIdent (getValue name)

prettifyEqualities :: Equalities -> String
prettifyEqualities Equalities { getKindEqualities = kindEqualities
                              , getSortEqualities = sortEqualities
                              , getHasSortEqualities = hasSortEqualities
                              } =
    unlines $
    [prettifyHeader "Kind equalities"] ++
    map (prettifyEquality prettifyKind prettifyKind) kindEqualities ++
    [prettifyHeader "Sort equalities"] ++
    map (prettifyEquality prettifySort prettifySort) sortEqualities ++
    [prettifyHeader "Has sort"] ++
    map (prettifyEquality prettifyKind prettifySort) hasSortEqualities

prettifyEquality :: (a -> String) -> (b -> String) -> (a, b) -> String
prettifyEquality prettifyFirst prettifySecond (first, second) =
    prettifyFirst first ++ " = " ++ prettifySecond second

prettifySubstitution :: (a -> String) -> Substitution a -> String
prettifySubstitution prettifyObject =
    let prettifySingle (ident, object) =
            prettifyIdent ident ++ "::=" ++ prettifyObject object
     in unlines . map prettifySingle . HM.toList
