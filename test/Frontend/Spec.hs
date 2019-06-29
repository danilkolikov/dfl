{- |
Module      :  Frontend.Spec
Description :  Tests for the frontend of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for the frontend of DFL
-}
module Frontend.Spec where

import qualified Frontend.Desugaring.Spec as Desugaring
import qualified Frontend.Inference.Spec as Inference
import qualified Frontend.Syntax.Spec as Syntax

testSuite :: IO ()
testSuite = do
    Desugaring.testSuite
    Inference.testSuite
    Syntax.testSuite
