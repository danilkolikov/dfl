{- |
Module      :  Util.DependencyResolver
Description :  Resolver of dependencies
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Resolver of dependencies between declarations
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Util.DependencyResolver where

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Control.Monad.Trans.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Trans.State (StateT, evalStateT, get, modify)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)

-- | Type for the set of dependency of some identifier
type Dependencies a = HS.HashSet a

-- | Graph of dependencies between identifiers
type DependencyGraph a = BipartiteGraph a a

-- | A bipartite graph
type BipartiteGraph a b = HM.HashMap a (Dependencies b)

-- | Inverses a graph
inverseGraph :: (Eq a, Hashable a) => DependencyGraph a -> DependencyGraph a
inverseGraph graph =
    let inversed = inverseBipartiteGraph graph
        missingNodes = filter (not . (`HM.member` inversed)) $ HM.keys graph
     in HM.fromList (zip missingNodes $ repeat HS.empty) `HM.union` inversed

-- | Construct an inverse bipartite graph and returns nodes which don't have edges
inverseBipartiteGraph ::
       (Eq a, Hashable a, Eq b, Hashable b)
    => HM.HashMap a (Dependencies b)
    -> HM.HashMap b (Dependencies a)
inverseBipartiteGraph = inverseGraph' . HM.toList
  where
    inverseGraph' [] = HM.empty
    inverseGraph' ((name, dependencies):rest) =
        let inversedRest = inverseGraph' rest
            inverse node graph =
                case HM.lookup node graph of
                    Nothing -> HM.insert node (HS.singleton name) graph
                    Just edges -> HM.insert node (HS.insert name edges) graph
         in HS.foldr inverse inversedRest dependencies

-- | Get nodes of the graph which have loops
getLoops :: (Eq a, Hashable a) => DependencyGraph a -> [a]
getLoops = map fst . filter (uncurry HS.member) . HM.toList

-- | Errors which can be encountered during dependency resolution
newtype DependencyResolverError a =
    DependencyResolverErrorUnknownNode a -- ^ Unknown identifier
    deriving (Eq, Show)

-- | A bipartite resolver
type BipartiteDependencyResolver a b
     = ReaderT (BipartiteGraph a b) (Except (DependencyResolverError a))

-- | Type for resolvers of dependencies
type DependencyResolver a = BipartiteDependencyResolver a a

-- | Run resolver of dependencies
runDependencyResolver ::
       BipartiteDependencyResolver a b c
    -> BipartiteGraph a b
    -> Either (DependencyResolverError a) c
runDependencyResolver vt graph = runExcept $ runReaderT vt graph

-- | Find node in the graph from the context
lookupNode ::
       (Eq a, Hashable a) => a -> BipartiteDependencyResolver a b (HS.HashSet b)
lookupNode node = do
    graph <- ask
    lift $ lookupNodeInBipartite graph node

-- | Find a node in a bipartite graph
lookupNodeInBipartite ::
       (Eq a, Hashable a)
    => BipartiteGraph a b
    -> a
    -> Except (DependencyResolverError a) (HS.HashSet b)
lookupNodeInBipartite graph node =
    case HM.lookup node graph of
        Nothing -> throwE $ DependencyResolverErrorUnknownNode node
        Just dependencies -> return dependencies

-- | Type for objects which track visited nodes in the graph
type VisitedTracker a b
     = StateT (HS.HashSet a) (ReaderT (DependencyGraph a) (Except (DependencyResolverError a))) b

-- | Run visited tracker
runVisitedTracker :: VisitedTracker a b -> DependencyResolver a b
runVisitedTracker vt = evalStateT vt HS.empty

-- | Do a depth first search in a graph, starting from the provided node, and
-- | return a list of visited nodes
depthFirstSearch :: (Eq a, Hashable a) => a -> VisitedTracker a [a]
depthFirstSearch node =
    get >>= \visited ->
        if node `HS.member` visited
            then return []
            else do
                modify $ HS.insert node
                dependencies <- lift $ lookupNode node
                sorted <- mapM depthFirstSearch (HS.toList dependencies)
                return $ concat sorted ++ [node]

-- | Sort the nodes in the graph topologically
topologicalSort :: (Eq a, Hashable a) => DependencyResolver a [a]
topologicalSort = do
    graph <- ask
    sorted <- runVisitedTracker . mapM depthFirstSearch $ HM.keys graph
    return . reverse . concat $ sorted

-- | Find nodes, reachable from the provided ones
findReachableNodes ::
       (Eq a, Hashable a) => [a] -> DependencyResolver a [HS.HashSet a]
findReachableNodes idents = do
    connected <- runVisitedTracker $ mapM depthFirstSearch idents
    return . map HS.fromList . filter (not . null) $ connected

-- | Find strongly connected components in the graph
stronglyConnectedComponents ::
       (Eq a, Hashable a) => DependencyResolver a [HS.HashSet a]
stronglyConnectedComponents =
    local inverseGraph topologicalSort >>= findReachableNodes

-- | Condense the graph, using the provided connectivity graph, where
-- | each node is connected only to the node, corresponding to a specific component
condenseGraph ::
       (Eq a, Hashable a)
    => BipartiteGraph Component a
    -> DependencyResolver a (DependencyGraph Component)
condenseGraph compGraph = do
    let nodeToName = inverseBipartiteGraph compGraph
        lookupNodeName = lookupNodeInBipartite nodeToName
        processSingle node = do
            dependencies <- lookupNode node
            names <- lift $ mapM lookupNodeName (HS.toList dependencies)
            return $ HS.unions names
        processComponent (name, connected) = do
            processed <- mapM processSingle (HS.toList connected)
            let withoutItself = HS.delete name (HS.unions processed)
            return $ HM.singleton name withoutItself
    processed <- mapM processComponent $ HM.toList compGraph
    return $ HM.unions processed

newtype Component =
    Component Int
    deriving (Eq, Hashable, Show)

-- | Topologically sort provided components
topologicalSortOfComponents ::
       (Eq a, Hashable a)
    => [HS.HashSet a]
    -> DependencyResolver a [HS.HashSet a]
topologicalSortOfComponents comps = do
    let componentsGraph =
            HM.fromList $
            zipWith (\pos comp -> (Component pos, comp)) [0 ..] comps
    condensedGraph <- condenseGraph componentsGraph
    -- All nodes should be defined, so there cannot be errors
    let (Right sorted) = runDependencyResolver topologicalSort condensedGraph
        (Right nodes) =
            runDependencyResolver (mapM lookupNode sorted) componentsGraph
    return nodes

-- | Find strongle connected components and topologically sort them
getSortedStrongConComp ::
       (Eq a, Hashable a) => DependencyResolver a [HS.HashSet a]
getSortedStrongConComp =
    stronglyConnectedComponents >>= topologicalSortOfComponents

-- | Find dependency groups in the provided graph and topologically sort them
getDependencyGroups ::
       (Eq a, Hashable a)
    => DependencyGraph a
    -> Either (DependencyResolverError a) [HS.HashSet a]
getDependencyGroups = runDependencyResolver getSortedStrongConComp

-- | Finds dependency groups in the graph and traverses them in the reverse order
traverseGraph ::
       (Eq a, Hashable a, Monad m)
    => (b -> HS.HashSet a -> m b)
    -> b
    -> DependencyGraph a
    -> Either (DependencyResolverError a) ([HS.HashSet a], m b)
traverseGraph f initial graph = do
    dependencyGroups <- getDependencyGroups graph
    let reversedOrder = reverse dependencyGroups
        traversed = foldM f initial reversedOrder
    return (dependencyGroups, traversed)
