{- |
Module      :  Frontend.Desugaring.Final.ExpressionDesugaringStmtTest
Description :  Tests for desugaring of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of statements
-}
module Frontend.Desugaring.Final.ExpressionDesugaringStmtTest
    ( testSuite
    ) where

import Test.Hspec

import Control.Monad (replicateM)

import Frontend.Desugaring.Final.Ast hiding (getDataTypeConstructors)
import Frontend.Desugaring.Final.ExpressionDesugaringBase
-- import Frontend.Desugaring.Final.ExpressionDesugaringStmt
import Frontend.Syntax.Position (withDummyLocation)

testSuite :: IO ()
testSuite =
    hspec $
    describe "generateNewIdent" $
    it "generates new idents" $
    runExpressionDesugaringProcessor (replicateM 5 generateNewIdent') 0 `shouldBe`
    ( Right
          [ withDummyLocation $
          IdentGenerated IdentEnvironmentExpressionDesugaring i
          | i <- [0 .. 4]
          ]
    , 5)
