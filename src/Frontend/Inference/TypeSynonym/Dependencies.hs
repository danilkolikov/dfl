{- |
Module      :  Frontend.Inference.TypeSynonym.Dependencies
Description :  Resolution of dependencies between type synonyms
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Function for resolution of dependencies between type synonyms
-}
module Frontend.Inference.TypeSynonym.Dependencies
    ( getTypeSynonymsDependencyGraph
    ) where

import Control.Monad (liftM2)
import Control.Monad.Trans.Reader (Reader, ask, runReader)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Final.Ast
import Frontend.Inference.DependencyResolver (Dependencies, DependencyGraph)
import Frontend.Syntax.Position (WithLocation(..))

-- | Gets the graph of dependencies between type synonyms
getTypeSynonymsDependencyGraph :: TypeSynonyms -> DependencyGraph
getTypeSynonymsDependencyGraph typeSynonyms =
    let definedTypeSynonyms = HM.keysSet typeSynonyms
        getSingleTypeSynonymDependencies typeSynonym =
            runReader
                (getTypeSynonymDependencies typeSynonym)
                definedTypeSynonyms
     in HM.map getSingleTypeSynonymDependencies typeSynonyms

-- | A type of a dependency getter
type DependencyGetter = Reader (HS.HashSet Ident) Dependencies

-- | Gets dependencies of a type synonym
getTypeSynonymDependencies :: TypeSynonym -> DependencyGetter
getTypeSynonymDependencies = getTypeDependencies . getTypeSynonymType

-- | Gets dependencies of a type
getTypeDependencies :: WithLocation Type -> DependencyGetter
getTypeDependencies type' =
    case getValue type' of
        TypeVar _ -> return HS.empty
        TypeConstr name -> getIdentDependencies name
        TypeFunction from to ->
            liftM2 HS.union (getTypeDependencies from) (getTypeDependencies to)
        TypeApplication func args -> do
            funcDependencies <- getTypeDependencies func
            argsDependencies <- mapM getTypeDependencies args
            return . HS.unions $ funcDependencies : NE.toList argsDependencies

-- | Gets dependencies of a single ident
getIdentDependencies :: WithLocation Ident -> DependencyGetter
getIdentDependencies name = do
    defined <- ask
    return $
        if HS.member (getValue name) defined
            then HS.singleton (getValue name)
            else HS.empty
