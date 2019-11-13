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
    -> WithLocation UserDefinedIdent
    -> Maybe (WithLocation UserDefinedIdent)
    -> Explicit
    -> NameMapping
createNameMapping isQualified _ asName imports
    | Explicit { getExplicitTypeSynonyms = typeSynonyms
               , getExplicitDataTypes = dataTypes
               , getExplicitClasses = classes
               , getExplicitExpressions = expressions
               } <- imports =
        NameMapping
            { getNameMappingTypes =
                  mconcat
                      [ createSingleMapping typeSynonyms
                      , createSingleMapping dataTypes
                      , createSingleMapping classes
                      ]
            , getNameMappingExpressions = createSingleMapping expressions
            }
  where
    createSingleMapping = mconcat . map processIdent . HM.keys
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
    createQualifiedPair ident = Just (ident, ident)
    createUnqualifiedPair ident =
        if isQualified
            then Nothing
            else case ident of
                     IdentQualified _ simple -> Just (IdentSimple simple, ident)
                     IdentSimple {} -> Nothing
    createRenamedPair ident =
        case asName of
            Nothing -> Nothing
            Just newModule ->
                let newPath =
                        case getValue newModule of
                            IdentQualified path simple ->
                                path ++ getSimplePath simple
                            IdentSimple simple -> getSimplePath simple
                    getSimplePath simple =
                        case simple of
                            IdentNamed name -> [name]
                            IdentParametrised {} ->
                                error "Unsupported ident type"
                 in Just $
                    case ident of
                        IdentQualified _ simple ->
                            (IdentQualified newPath simple, ident)
                        IdentSimple simple ->
                            (IdentQualified newPath simple, ident)
