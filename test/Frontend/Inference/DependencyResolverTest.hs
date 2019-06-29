{- |
Module      :  Frontend.Inference.DependencyResolverTest
Description :  Tests for the dependency resolver
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Tests for the dependency resolver
-}
module Frontend.Inference.DependencyResolverTest
    ( testSuite
    ) where

import Test.Hspec

import Data.Bifunctor (bimap)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Frontend.Desugaring.Final.Ast (Ident(..))
import Frontend.Inference.DependencyResolver

testSuite :: IO ()
testSuite =
    hspec $ do
        let node = IdentNamed . return
            makeNodeSet = HS.fromList . map node
            makeComponents = map makeNodeSet
            makeGraph = HM.fromList . map (bimap node makeNodeSet)
        describe "inverseGraph" $ do
            let graph =
                    makeGraph
                        [ ("a", ["b", "c", "d"])
                        , ("b", ["a", "e"])
                        , ("d", ["c"])
                        ]
                inversed =
                    makeGraph
                        [ ("a", ["b"])
                        , ("b", ["a"])
                        , ("c", ["a", "d"])
                        , ("d", ["a"])
                        , ("e", ["b"])
                        ]
            it "inverses graphs" $ do
                inverseGraph graph `shouldBe` inversed
                inverseGraph inversed `shouldBe` graph
        describe "lookupNode" $ do
            let graph = makeGraph [("a", ["a"])]
            it "finds an existing node" $
                runDependencyResolver (lookupNode (node "a")) graph `shouldBe`
                Right (HS.fromList [node "a"])
            it "raises error when a node is not found" $
                runDependencyResolver (lookupNode (node "b")) graph `shouldBe`
                Left (DependencyResolverErrorUnknownNode (node "b"))
        describe "topologicalSort" $ do
            let graph =
                    makeGraph
                        [ ("a", ["b", "c"])
                        , ("b", ["d"])
                        , ("c", ["b"])
                        , ("d", [])
                        , ("e", ["f", "g"])
                        , ("f", [])
                        , ("g", ["h"])
                        , ("h", ["h", "e"])
                        ]
                sorted = map node ["a", "c", "b", "e", "g", "h", "f", "d"]
            it "sorts graphs topologically" $
                runDependencyResolver topologicalSort graph `shouldBe`
                Right sorted
        describe "findReachableNodes" $ do
            let graph =
                    makeGraph
                        [ ("a", ["b", "c", "d"])
                        , ("b", ["d"])
                        , ("c", [])
                        , ("d", [])
                        , ("e", ["f"])
                        , ("f", [])
                        ]
                nodes = map node ["b", "c", "d", "e", "f", "a"]
                reachable =
                    makeComponents
                        [ ["b", "d"]
                        , ["c"]
                        , ["e", "f"]
                        , ["a"]
                        ]
            it "finds reachable nodes" $
                runDependencyResolver (findReachableNodes nodes) graph `shouldBe`
                Right reachable
        let graph =
                makeGraph
                    [ ("a", ["b"])
                    , ("b", ["c", "e"])
                    , ("c", ["a", "d", "h"])
                    , ("d", ["c"])
                    , ("e", ["f"])
                    , ("f", ["g"])
                    , ("g", ["e"])
                    , ("h", ["f"])
                    ]
            comps =
                makeComponents [["e", "f", "g"], ["h"], ["a", "b", "c", "d"]]
        describe "stronglyConnectedComponents" $ do
            it "finds stronlgy connected components" $
                runDependencyResolver stronglyConnectedComponents graph `shouldBe`
                Right comps
        describe "condenseGraph" $ do
            let componentsGraph =
                    makeGraph
                        [ ("1", ["a", "b", "c", "d"])
                        , ("2", ["e", "f", "g"])
                        , ("3", ["h"])
                        ]
                condensed =
                    makeGraph [("1", ["2", "3"]), ("2", []), ("3", ["2"])]
            it "condenses graph" $
                runDependencyResolver (condenseGraph componentsGraph) graph `shouldBe`
                Right condensed
        let sorted =
                makeComponents [["a", "b", "c", "d"], ["h"], ["e", "f", "g"]]
        describe "topologicalSortOfComponents" $ do
            it "topologically sorts components" $ do
                runDependencyResolver
                    (topologicalSortOfComponents comps)
                    graph `shouldBe`
                    Right sorted
        describe "getDependencyGroups" $ do
            it "finds and orders dependency groups" $
                getDependencyGroups graph `shouldBe` Right sorted
