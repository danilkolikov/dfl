{- |
Module      :  Frontend.Desugaring.Final.BaseTest
Description :  Tests for desugaring of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of expressions
-}
module Frontend.Desugaring.Final.BaseTest
    ( testSuite
    ) where

import Test.Hspec

import Control.Monad (replicateM)

import Frontend.Desugaring.Final.Ast hiding (getDataTypeConstructors)
import Frontend.Desugaring.Final.Base
import Frontend.Syntax.Position (withDummyLocation)

testSuite :: IO ()
testSuite =
    hspec $
    describe "generateNewIdent" $
    it "generates new idents" $
    runExpressionDesugaringProcessor (replicateM 5 generateNewIdent) 0 `shouldBe`
    [ withDummyLocation $
    IdentGenerated $
    GeneratedIdent GeneratedIdentEnvironmentExpressionDesugaring i
    | i <- [0 .. 4]
    ]
