{- |
Module      :  Frontend.Inference.TypeSynonyms.Processor
Description :  Functions for expanding of type synonyms
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of expanding of type synonyms inference
-}
module Frontend.Inference.TypeSynonyms.Processor where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, except, runExcept, throwE)
import Control.Monad.Trans.State.Lazy (StateT, execStateT, get, modify)
import Data.Bifunctor (first)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
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
type TypeSynonymProcessor
     = StateT TypeSynonymSignatures (Except TypeSynonymsProcessingError)

-- | Processes type synonyms and expands their types
processSignatures ::
       F.TypeSynonyms
    -> HM.HashMap Ident TypeConstructorSignature
    -> Either TypeSynonymsProcessingError TypeSynonymSignatures
processSignatures synonyms typeSynonymSignatures =
    runExcept $
    execStateT (processTypeSynonyms synonyms typeSynonymSignatures) HM.empty

-- | Processes type synonyms
processTypeSynonyms ::
       F.TypeSynonyms
    -> HM.HashMap Ident TypeConstructorSignature
    -> TypeSynonymProcessor ()
processTypeSynonyms typeSynonyms typeSignatures =
    let graph = getTypeSynonymsDependencyGraph typeSynonyms
        loops = getLoops graph
        assertNull list = unless (null list) . lift . throwE
        lookupTypeSynonym name =
            ( fromJust $ HM.lookup name typeSynonyms
            , fromJust $ HM.lookup name typeSignatures)
     in do assertNull loops $ TypeSynonymsProcessingErrorRecursive (head loops)
           groups <-
               wrapError TypeSynonymsProcessingErrorDependencyResolution $
               getDependencyGroups graph
           let mutuallyRecursiveGroups = filter (\g -> length g > 1) groups
           assertNull mutuallyRecursiveGroups $
               TypeSynonymsProcessingErrorMutuallyRecursive
                   (HS.toList . head $ mutuallyRecursiveGroups)
           let groupItems = map (head . HS.toList) (reverse groups)
           mapM_ (uncurry processTypeSynonym . lookupTypeSynonym) groupItems

-- | Processes a single type synonym
processTypeSynonym ::
       F.TypeSynonym -> TypeConstructorSignature -> TypeSynonymProcessor ()
processTypeSynonym F.TypeSynonym { F.getTypeSynonymName = name
                                 , F.getTypeSynonymType = type'
                                 } signature = do
    definedSynonyms <- get
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
    modify $ HM.insert (getValue name) typeSignature

-- | Wraps an error
wrapError ::
       (a -> TypeSynonymsProcessingError)
    -> Either a b
    -> TypeSynonymProcessor b
wrapError wrap = lift . except . first wrap
