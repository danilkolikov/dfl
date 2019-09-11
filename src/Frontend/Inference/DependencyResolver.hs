{- |
Module      :  Frontend.Inference.DependencyResolver
Description :  Resolver of dependencies
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Resolver of dependencies between declarations
-}
module Frontend.Inference.DependencyResolver where

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Control.Monad.Trans.Reader (ReaderT, ask, local, runReaderT)
import Control.Monad.Trans.State (StateT, evalStateT, get, modify)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Frontend.Desugaring.Final.Ast (Ident(..), IdentEnvironment(..))

-- | Type for the set of dependency of some identifier
type Dependencies = HS.HashSet Ident

-- | Graph of dependencies between identifiers
type DependencyGraph = HM.HashMap Ident Dependencies

-- | Construct an inverse graph
inverseGraph :: DependencyGraph -> DependencyGraph
inverseGraph = inverseGraph' . HM.toList
  where
    inverseGraph' :: [(Ident, Dependencies)] -> DependencyGraph
    inverseGraph' [] = HM.empty
    inverseGraph' ((name, dependencies):rest) =
        let inversedRest = inverseGraph' rest
            restWithNode =
                if HM.member name inversedRest
                    then inversedRest
                    else HM.insert name HS.empty inversedRest
            inverse node graph =
                case HM.lookup node graph of
                    Nothing -> HM.insert node (HS.singleton name) graph
                    Just edges -> HM.insert node (HS.insert name edges) graph
         in HS.foldr inverse restWithNode dependencies

-- | Get nodes of the graph which have loops
getLoops :: DependencyGraph -> [Ident]
getLoops = map fst . filter (uncurry HS.member) . HM.toList

-- | Errors which can be encountered during dependency resolution
newtype DependencyResolverError =
    DependencyResolverErrorUnknownNode Ident -- ^ Unknown identifier
    deriving (Eq, Show)

-- | Type for resolvers of dependencies
type DependencyResolver a
     = ReaderT DependencyGraph (Except DependencyResolverError) a

-- | Run resolver of dependencies
runDependencyResolver ::
       DependencyResolver a
    -> DependencyGraph
    -> Either DependencyResolverError a
runDependencyResolver vt graph = runExcept $ runReaderT vt graph

-- | Find node in the graph
lookupNode :: Ident -> DependencyResolver (HS.HashSet Ident)
lookupNode node = do
    graph <- ask
    case HM.lookup node graph of
        Nothing -> lift . throwE $ DependencyResolverErrorUnknownNode node
        Just dependencies -> return dependencies

-- | Type for objects which track visited nodes in the graph
type VisitedTracker a
     = StateT (HS.HashSet Ident) (ReaderT DependencyGraph (Except DependencyResolverError)) a

-- | Run visited tracker
runVisitedTracker :: VisitedTracker a -> DependencyResolver a
runVisitedTracker vt = evalStateT vt HS.empty

-- | Do a depth first search in a graph, starting from the provided node, and
-- | return a list of visited nodes
depthFirstSearch :: Ident -> VisitedTracker [Ident]
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
topologicalSort :: DependencyResolver [Ident]
topologicalSort = do
    graph <- ask
    sorted <- runVisitedTracker . mapM depthFirstSearch $ HM.keys graph
    return . reverse . concat $ sorted

-- | Find nodes, reachable from the provided ones
findReachableNodes :: [Ident] -> DependencyResolver [HS.HashSet Ident]
findReachableNodes idents = do
    connected <- runVisitedTracker $ mapM depthFirstSearch idents
    return . map HS.fromList . filter (not . null) $ connected

-- | Find strongly connected components in the graph
stronglyConnectedComponents :: DependencyResolver [HS.HashSet Ident]
stronglyConnectedComponents =
    local inverseGraph topologicalSort >>= findReachableNodes

-- | Condense the graph, using the provided connectivity graph, where
-- | each node is connected only to the node, corresponding to a specific component
condenseGraph :: DependencyGraph -> DependencyResolver DependencyGraph
condenseGraph compGraph = do
    let nodeToName = inverseGraph compGraph
        lookupNodeName = local (const nodeToName) . lookupNode
        processSingle :: Ident -> DependencyResolver (HS.HashSet Ident)
        processSingle node = do
            dependencies <- lookupNode node
            names <- mapM lookupNodeName (HS.toList dependencies)
            return $ HS.unions names
        processComponent ::
               (Ident, HS.HashSet Ident) -> DependencyResolver DependencyGraph
        processComponent (name, connected) = do
            processed <- mapM processSingle (HS.toList connected)
            let withoutItself = HS.delete name (HS.unions processed)
            return $ HM.singleton name withoutItself
    processed <- mapM processComponent $ HM.toList compGraph
    return $ HM.unions processed

-- | Topologically sort provided components
topologicalSortOfComponents ::
       [HS.HashSet Ident] -> DependencyResolver [HS.HashSet Ident]
topologicalSortOfComponents comps = do
    let makeIdent = IdentGenerated IdentEnvironmentDependencyResolution
        namedComponents =
            zipWith (\pos comp -> (makeIdent pos, comp)) [0 ..] comps
        compGraph = HM.fromList namedComponents
        lookupComponent = local (const compGraph) . lookupNode
    condensedGraph <- condenseGraph compGraph
    sorted <- local (const condensedGraph) topologicalSort
    mapM lookupComponent sorted

-- | Find strongle connected components and topologically sort them
getSortedStrongConComp :: DependencyResolver [HS.HashSet Ident]
getSortedStrongConComp =
    stronglyConnectedComponents >>= topologicalSortOfComponents

-- | Find dependency groups in the provided graph and topologically sort them
getDependencyGroups ::
       DependencyGraph -> Either DependencyResolverError [HS.HashSet Ident]
getDependencyGroups = runDependencyResolver getSortedStrongConComp

-- | Finds dependency groups in the graph and traverses them in the reverse order 
traverseGraph ::
       (Monad m)
    => (a -> [Ident] -> m a)
    -> a
    -> DependencyGraph
    -> Either DependencyResolverError ([HS.HashSet Ident], m a)
traverseGraph f initial graph = do
    dependencyGroups <- getDependencyGroups graph
    let reversedOrder = map HS.toList $ reverse dependencyGroups
        traversed = foldM f initial reversedOrder
    return (dependencyGroups, traversed)
