{- |
Module      :  Frontend.Inference.TypeSynonyms.Processor
Description :  Functions for expanding of type synonyms
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of expanding of type synonyms inference
-}
module Frontend.Inference.TypeSynonyms.Processor where

import Control.Monad (unless)
import Data.Bifunctor (first)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromJust)

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.DependencyResolver
import Frontend.Inference.Signature
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

-- | A map of signatures of type synonyms
type TypeSynonymSignatures = HM.HashMap Ident TypeSignature

-- | A type of objects which process type synonyms
type TypeSynonymProcessor = Either TypeSynonymsProcessingError

-- | Processes type synonyms and expands their types
processSignatures ::
       TypeSynonymSignatures
    -> F.TypeSynonyms
    -> HM.HashMap Ident TypeConstructorSignature
    -> TypeSynonymProcessor TypeSynonymSignatures
processSignatures initialSignatures typeSynonyms typeSignatures = do
    let graph = getTypeSynonymsDependencyGraph typeSynonyms
        loops = getLoops graph
        lookupTypeSynonym name =
            ( fromJust $ HM.lookup name typeSynonyms
            , fromJust $ HM.lookup name typeSignatures)
    unless (null loops) . Left $
        TypeSynonymsProcessingErrorRecursive (head loops)
    (_, processed) <-
        wrapError TypeSynonymsProcessingErrorDependencyResolution $
        traverseGraph
            (processDependencyGroup lookupTypeSynonym)
            initialSignatures
            graph
    processed

-- | Processes a single dependency group
processDependencyGroup ::
       (F.Ident -> (F.TypeSynonym, TypeConstructorSignature))
    -> TypeSynonymSignatures
    -> [F.Ident]
    -> TypeSynonymProcessor TypeSynonymSignatures
processDependencyGroup lookupTypeSynonym signatures group = do
    unless (length group == 1) . Left $
        TypeSynonymsProcessingErrorMutuallyRecursive group
    processTypeSynonym signatures . lookupTypeSynonym $ head group

-- | Processes a single type synonym
processTypeSynonym ::
       TypeSynonymSignatures
    -> (F.TypeSynonym, TypeConstructorSignature)
    -> TypeSynonymProcessor TypeSynonymSignatures
processTypeSynonym definedSynonyms (F.TypeSynonym { F.getTypeSynonymName = name
                                                  , F.getTypeSynonymType = type'
                                                  }, signature) = do
    let expanded = expandTypeSynonyms type' definedSynonyms
    processedType <- wrapError TypeSynonymsProcessingErrorExpanding expanded
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

-- | Wraps an error
wrapError ::
       (a -> TypeSynonymsProcessingError)
    -> Either a b
    -> TypeSynonymProcessor b
wrapError = first
