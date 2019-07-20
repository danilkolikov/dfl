{- |
Module      :  Frontend.Inference.Kind.DependencyGroupResolver
Description :  Resolver of items in dependency groups
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for resolving of items in dependency groups
-}
module Frontend.Inference.Kind.DependencyGroupResolver
    ( resolveDependencyGroup
    ) where

import Control.Monad.Trans.Reader (Reader, ask, runReader)
import Data.Foldable (asum)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromMaybe)

import Frontend.Desugaring.Final.Ast (Ident)
import Frontend.Inference.Kind.ProcessorBase

-- | Resolve identifiers in a single dependency group
resolveDependencyGroup :: [Ident] -> Environment -> [DependencyGroupItemEmpty]
resolveDependencyGroup group = runReader (resolveMany group)

-- | Resolver of identifiers in a group
type Resolver a = Reader Environment a

-- | Resolve many identifiers
resolveMany :: [Ident] -> Resolver [DependencyGroupItemEmpty]
resolveMany = mapM resolveSingle

-- | Resolve a single identifier
resolveSingle :: Ident -> Resolver DependencyGroupItemEmpty
resolveSingle name = do
    env <- ask
    let maybeResolveSingle getMap wrapResult =
            (`wrapResult` ()) <$> HM.lookup name (getMap env)
        prepared =
            asum
                [ maybeResolveSingle
                      getTypeSynonyms
                      DependencyGroupItemTypeSynonym
                , maybeResolveSingle getDataTypes DependencyGroupItemDataType
                , maybeResolveSingle getClasses DependencyGroupItemClass
                ]
    -- This error should not occur, because we expect that all idents are
    -- either type synonyms, data types or classes
    return $ fromMaybe (error $ "Unexpected identifier " ++ show name) prepared
