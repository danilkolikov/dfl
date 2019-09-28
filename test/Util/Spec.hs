{- |
Module      :  Util.Spec
Description :  Tests for inference of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for inference of DFL
-}
module Util.Spec
    ( testSuite
    ) where

import qualified Util.DependencyResolverTest as DependencyResolver

testSuite :: IO ()
testSuite = do
    DependencyResolver.testSuite
