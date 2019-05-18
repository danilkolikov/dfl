{- |
Module      :  Frontend.Spec
Description :  Tests for the frontend of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for the frontend of DFL
-}
module Frontend.Spec where

import Frontend.Syntax.Spec as Syntax

testSuite :: IO ()
testSuite = do
    Syntax.testSuite
