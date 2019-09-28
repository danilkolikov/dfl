{- |
Module      :  Frontend.Inference.TypeSynonym.Base
Description :  Base functions for expanding type synonyms
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base functions for expanding type synonyms
-}
module Frontend.Inference.TypeSynonym.Base
    ( processSignatures
    , TypeSynonymProcessorError(..)
    ) where

import Control.Monad (unless)
import Data.Bifunctor (first)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Signature
import Frontend.Inference.Type
import Frontend.Inference.TypeSynonym.Dependencies
import Frontend.Inference.TypeSynonym.Expand
import Frontend.Inference.Variables
import Frontend.Syntax.Position
import Util.DependencyResolver

-- | Errors which can be encountered during processing of type synonyms
data TypeSynonymProcessorError
    = TypeSynonymProcessorErrorRecursive Ident -- ^ Recursive type synonym
    | TypeSynonymProcessorErrorMutuallyRecursive [Ident] -- ^ Mutually recursive type synonyms
    | TypeSynonymProcessorErrorDependencyResolution (DependencyResolverError Ident) -- ^ Error happened during dependency resolution
    | TypeSynonymProcessorErrorExpanding TypeSynonymExpandingError -- ^ Error happened during expanding
    deriving (Eq, Show)

-- | Processes type synonyms and expands their types
processSignatures ::
       HM.HashMap Ident TypeConstructorSignature
    -> Signatures TypeSignature
    -> F.TypeSynonyms
    -> Either TypeSynonymProcessorError (Signatures TypeSignature)
processSignatures typeSignatures initialSignatures typeSynonyms = do
    let graph = getTypeSynonymsDependencyGraph typeSynonyms
        loops = getLoops graph
        lookupTypeSynonym name =
            ( fromJust $ HM.lookup name typeSynonyms
            , fromJust $ HM.lookup name typeSignatures)
    unless (null loops) . Left $ TypeSynonymProcessorErrorRecursive (head loops)
    (_, processed) <-
        first TypeSynonymProcessorErrorDependencyResolution $
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
    -> Either TypeSynonymProcessorError (Signatures TypeSignature)
processDependencyGroup lookupTypeSynonym signatures group = do
    let groupList = HS.toList group
    unless (length groupList == 1) . Left $
        TypeSynonymProcessorErrorMutuallyRecursive groupList
    processTypeSynonym signatures . lookupTypeSynonym $ head groupList

-- | Processes a single type synonym
processTypeSynonym ::
       Signatures TypeSignature
    -> (F.TypeSynonym, TypeConstructorSignature)
    -> Either TypeSynonymProcessorError (Signatures TypeSignature)
processTypeSynonym definedSynonyms (F.TypeSynonym { F.getTypeSynonymName = name
                                                  , F.getTypeSynonymType = type'
                                                  }, signature) = do
    let expanded =
            expandTypeSynonyms definedSynonyms $ removePositionsOfType type'
    processedType <- first TypeSynonymProcessorErrorExpanding expanded
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
