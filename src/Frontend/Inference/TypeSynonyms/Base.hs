{- |
Module      :  Frontend.Inference.TypeSynonyms.Base
Description :  Base functions for expanding type synonyms
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base functions for expanding type synonyms
-}
module Frontend.Inference.TypeSynonyms.Base
    ( processSignatures
    , TypeSynonymsProcessingError(..)
    ) where

import Control.Monad (unless)
import Data.Bifunctor (first)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.DependencyResolver
import Frontend.Inference.Signature
import Frontend.Inference.Type
import Frontend.Inference.TypeSynonyms.Dependencies
import Frontend.Inference.TypeSynonyms.Expand
import Frontend.Inference.Variables
import Frontend.Syntax.Position

-- | Errors which can be encountered during processing of type synonyms
data TypeSynonymsProcessingError
    = TypeSynonymsProcessingErrorRecursive Ident -- ^ Recursive type synonym
    | TypeSynonymsProcessingErrorMutuallyRecursive [Ident] -- ^ Mutually recursive type synonyms
    | TypeSynonymsProcessingErrorDependencyResolution DependencyResolverError -- ^ Error happened during dependency resolution
    | TypeSynonymsProcessingErrorExpanding TypeSynonymsExpandingError -- ^ Error happened during expanding
    deriving (Eq, Show)

-- | Processes type synonyms and expands their types
processSignatures ::
       HM.HashMap Ident TypeConstructorSignature
    -> Signatures TypeSignature
    -> F.TypeSynonyms
    -> Either TypeSynonymsProcessingError (Signatures TypeSignature)
processSignatures typeSignatures initialSignatures typeSynonyms = do
    let graph = getTypeSynonymsDependencyGraph typeSynonyms
        loops = getLoops graph
        lookupTypeSynonym name =
            ( fromJust $ HM.lookup name typeSynonyms
            , fromJust $ HM.lookup name typeSignatures)
    unless (null loops) . Left $
        TypeSynonymsProcessingErrorRecursive (head loops)
    (_, processed) <-
        first TypeSynonymsProcessingErrorDependencyResolution $
        traverseGraph
            (processDependencyGroup lookupTypeSynonym)
            initialSignatures
            graph
    processed

-- | Processes a single dependency group
processDependencyGroup ::
       (F.Ident -> (F.TypeSynonym, TypeConstructorSignature))
    -> Signatures TypeSignature
    -> HS.HashSet F.Ident
    -> Either TypeSynonymsProcessingError (Signatures TypeSignature)
processDependencyGroup lookupTypeSynonym signatures group = do
    let groupList = HS.toList group
    unless (length groupList == 1) . Left $
        TypeSynonymsProcessingErrorMutuallyRecursive groupList
    processTypeSynonym signatures . lookupTypeSynonym $ head groupList

-- | Processes a single type synonym
processTypeSynonym ::
       Signatures TypeSignature
    -> (F.TypeSynonym, TypeConstructorSignature)
    -> Either TypeSynonymsProcessingError (Signatures TypeSignature)
processTypeSynonym definedSynonyms (F.TypeSynonym { F.getTypeSynonymName = name
                                                  , F.getTypeSynonymType = type'
                                                  }, signature) = do
    let expanded =
            expandTypeSynonyms definedSynonyms $ removePositionsOfType type'
    processedType <- first TypeSynonymsProcessingErrorExpanding expanded
    let typeSignature =
            TypeSignature
                { getTypeSignatureSort = getSort signature
                , getTypeSignatureKindParams = getKindParams signature
                , getTypeSignatureKind = getKind signature
                , getTypeSignatureTypeParams = getTypeParams signature
                , getTypeSignatureType = processedType
                , getTypeSignatureContext = [] -- Type synonyms have empty context
                }
    return $ HM.insert (getValue name) typeSignature definedSynonyms
