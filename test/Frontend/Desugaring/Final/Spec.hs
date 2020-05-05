{- |
Module      :  Frontend.Desugaring.Final.Spec
Description :  Tests for desugaring of DFL grammar
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for the final step of desugaring of DFL grammar
-}
module Frontend.Desugaring.Final.Spec
    ( testSuite
    ) where

import qualified Frontend.Desugaring.Final.BaseTest as Base
import qualified Frontend.Desugaring.Final.CaseTest as Case
import qualified Frontend.Desugaring.Final.StmtTest as Stmt

testSuite :: IO ()
testSuite = do
    Base.testSuite
    Case.testSuite
    Stmt.testSuite
