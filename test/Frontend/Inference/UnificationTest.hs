{- |
Module      :  Frontend.Inference.UnificationTest
Description :  Tests for unification of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Tests for unification of expressions
-}
module Frontend.Inference.UnificationTest
    ( testSuite
    ) where

import Test.Hspec

import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Final.Ast (Ident(..))
import Frontend.Inference.AlgebraicExp
import Frontend.Inference.Unification

testSuite :: IO ()
testSuite =
    hspec $ do
        let ident = IdentNamed . return
            var = AlgebraicExpVar . ident
            func name = AlgebraicExpFunc (ident name)
        describe "unifyVar" $ do
            it "raises error when the variable is not free" $ do
                let variable = ident "x"
                    rhs = func "foo" [var "x"]
                unifyVar variable rhs `shouldBe`
                    Left (UnificationErrorRecursive variable rhs)
            it "ignores the same variables" $
                unifyVar (ident "x") (var "x") `shouldBe` Right HM.empty
            it "saves result otherwise" $ do
                let lhs = ident "x"
                    rhs = func "foo" []
                unifyVar lhs rhs `shouldBe` Right (HM.singleton lhs rhs)
        describe "unifySingle" $ do
            it "unifies variables" $ do
                let lhs = var "x"
                    rhs = func "foo" []
                    result = Right (HM.singleton (ident "x") rhs)
                unifySingle lhs rhs `shouldBe` result
                unifySingle rhs lhs `shouldBe` result
            it "unified functions" $ do
                let f1 = func "foo" []
                    f2 = func "foo" []
                    result = Right HM.empty
                unifySingle f1 f2 `shouldBe` result
            it "raises error when functions are different" $ do
                let f1 = func "foo" []
                    f2 = func "bar" []
                unifySingle f1 f2 `shouldBe`
                    Left
                        (UnificationErrorFunctionNameMismatch
                             (ident "foo")
                             (ident "bar"))
        describe "unifyArgs" $ do
            it "sequentially unifies args" $ do
                let leftArg1 = var "x"
                    leftArg2 = func "foo" [var "x"]
                    rightArg1 = func "bar" []
                    rightArg2 = func "foo" [var "y"]
                unifyArgs [leftArg1, leftArg2] [rightArg1, rightArg2] `shouldBe`
                    Right
                        (HM.fromList
                             [ (ident "x", func "bar" [])
                             , (ident "y", func "bar" [])
                             ])
            it "raises error when there are differnt number of args" $
                unifyArgs [] [var "x"] `shouldBe`
                Left UnificationErrorDifferentNumberOfArgs
        describe "unifyAlgebraicEqualities" $
            it "sequentially unifies algebraic equalities" $ do
                let eq1 = (var "x", func "bar" [])
                    eq2 = (func "foo" [var "x"], func "foo" [func "bar" []])
                unifyAlgebraicEqualities [eq1, eq2] `shouldBe`
                    Right (HM.singleton (ident "x") (func "bar" []))
