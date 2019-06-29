{- |
Module      :  Frontend.Inference.Spec
Description :  Tests for inference of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for inference of DFL
-}
module Frontend.Inference.Spec
    ( testSuite
    ) where

import qualified Frontend.Inference.DependencyResolverTest as DependencyResolver

testSuite :: IO ()
testSuite = do
    DependencyResolver.testSuite
