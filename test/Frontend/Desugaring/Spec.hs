{- |
Module      :  Frontend.Desugaring.Spec
Description :  Tests for desugaring of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of DFL
-}
module Frontend.Desugaring.Spec where

import qualified Frontend.Desugaring.Initial.Spec as Initial
import qualified Frontend.Desugaring.Final.Spec as Final

testSuite :: IO ()
testSuite = do
    Initial.testSuite
    Final.testSuite
