{- |
Module      :  Frontend.Module.Import.NameMapping
Description :  Creation of name mapping
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for creating mapping between imported objects and their names
-}
module Frontend.Module.Import.NameMapping
    ( createNameMapping
    ) where

import Data.Bifunctor (bimap)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe (catMaybes)

import Frontend.Module.Base
import Frontend.Syntax.Position

-- | Creates a mapping between names of imported objects and objects
createNameMapping ::
       Bool
    -> UserDefinedIdent
    -> Maybe (WithLocation UserDefinedIdent)
    -> Explicit
    -> NameMapping
createNameMapping isQualified moduleName asName imports
    | Explicit { getExplicitTypeSynonyms = typeSynonyms
               , getExplicitDataTypes = dataTypes
               , getExplicitClasses = classes
               , getExplicitExpressions = expressions
               } <- imports =
        let typeNameMapping =
                mconcat
                    [ createSingleMapping typeSynonyms
                    , createSingleMapping dataTypes
                    , createSingleMapping classes
                    ]
            expressionNameMapping =
                mconcat
                    [ createSingleMapping expressions
                    , createComponentsMapping getDataTypeConstructors dataTypes
                    , createComponentsMapping getClassMethods classes
                    ]
            modules = selectModules typeNameMapping expressionNameMapping
         in NameMapping
                { getNameMappingTypes = typeNameMapping
                , getNameMappingExpressions = expressionNameMapping
                , getNameMappingModules = modules
                }
  where
    createSingleMapping = mconcat . map processIdent . HM.keys
    createComponentsMapping getComponents =
        mconcat . map (createSingleMapping . getComponents) . HM.elems
    processIdent name =
        case name of
            IdentGenerated {} -> HM.empty -- Don't create mapping for generated idents
            IdentUserDefined ident ->
                let qualified = createQualifiedPair ident
                    unqualified = createUnqualifiedPair ident
                    renamed = createRenamedPair ident
                    pairs = catMaybes [qualified, unqualified, renamed]
                    identPairs =
                        map
                            (bimap
                                 IdentUserDefined
                                 (HS.singleton . IdentUserDefined))
                            pairs
                 in HM.fromList identPairs
    createQualifiedPair ident = Just (qualifyIdent moduleName ident, ident)
    createUnqualifiedPair ident =
        if isQualified
            then Nothing
            else case ident of
                     IdentQualified _ simple -> Just (IdentSimple simple, ident)
                     IdentSimple {} -> Nothing
    qualifyIdent newModule ident =
        let newPath =
                case newModule of
                    IdentQualified path simple -> path ++ getSimplePath simple
                    IdentSimple simple -> getSimplePath simple
            getSimplePath simple =
                case simple of
                    IdentNamed name -> [name]
                    IdentParametrised {} -> error "Unsupported ident type"
         in case ident of
                IdentQualified _ simple -> IdentQualified newPath simple
                IdentSimple simple -> IdentQualified newPath simple
    createRenamedPair ident =
        case asName of
            Nothing -> Nothing
            Just newModule ->
                Just (qualifyIdent (getValue newModule) ident, ident)
    selectModules types expressions =
        let unqualifiedTypes = selectUnqualifiedNames types
            unqualifiedExpressions = selectUnqualifiedNames expressions
         in HM.singleton
                (IdentUserDefined moduleName)
                (unqualifiedTypes <> unqualifiedExpressions)
    selectUnqualifiedNames mapping =
        let names = HM.keysSet mapping
            isQualifiedIdent ident
                | IdentUserDefined IdentQualified {} <- ident = True
                | otherwise = False
            unqualify ident
                | IdentUserDefined (IdentQualified _ simple) <- ident =
                    IdentUserDefined $ IdentSimple simple
                | otherwise = ident
            unqualifiedNames =
                HS.map unqualify $ HS.filter isQualifiedIdent names
            presentUnqualifiedNames = names `HS.intersection` unqualifiedNames
            mapUDIdent f (IdentUserDefined x) = IdentUserDefined (f x)
            mapUDIdent _ i = i
         in HS.map
                (mapUDIdent $ qualifyIdent moduleName)
                presentUnqualifiedNames
