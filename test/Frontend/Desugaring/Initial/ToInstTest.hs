{- |
Module      :  Frontend.Desugaring.Initial.ToInstTest
Description :  Tests for desugaring of object to Inst-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to Inst-s
-}
module Frontend.Desugaring.Initial.ToInstTest
    ( testSuite
    ) where

import Test.Hspec

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToInst (desugarToInst)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position
    ( WithLocation(..)
    , sourceLocation
    , withDummyLocation
    )
import Frontend.Syntax.Token

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToInst" $ do
        it "should desugar Inst" $ do
            desugarToInst
                (withDummyLocation $
                 InstNamed
                     (withDummyLocation GTyConUnit)
                     [withDummyLocation $ VarId "a"]) `shouldBe`
                withDummyLocation
                    (D.Inst
                         (withDummyLocation $ D.IdentNamed ["()"])
                         [withDummyLocation $ D.IdentNamed ["a"]])
            desugarToInst
                (withDummyLocation $
                 InstTuple
                     (withDummyLocation $ VarId "a")
                     (withDummyLocation $ VarId "b")
                     []) `shouldBe`
                withDummyLocation
                    (D.Inst
                         (withDummyLocation $ D.IdentParametrised ["(,)"] 2)
                         [ withDummyLocation $ D.IdentNamed ["a"]
                         , withDummyLocation $ D.IdentNamed ["b"]
                         ])
            desugarToInst
                (withDummyLocation $ InstList (withDummyLocation $ VarId "a")) `shouldBe`
                withDummyLocation
                    (D.Inst
                         (withDummyLocation $ D.IdentNamed ["[]"])
                         [withDummyLocation $ D.IdentNamed ["a"]])
            desugarToInst
                (withDummyLocation $
                 InstFunction
                     (withDummyLocation $ VarId "a")
                     (withDummyLocation $ VarId "b")) `shouldBe`
                withDummyLocation
                    (D.Inst
                         (withDummyLocation $ D.IdentNamed ["->"])
                         [ withDummyLocation $ D.IdentNamed ["a"]
                         , withDummyLocation $ D.IdentNamed ["b"]
                         ])
        it "should keep track of locations" $
            desugarToInst
                (WithLocation
                     (InstNamed (withDummyLocation GTyConUnit) [])
                     (sourceLocation 1 2 3 4)) `shouldBe`
            WithLocation
                (D.Inst (withDummyLocation $ D.IdentNamed ["()"]) [])
                (sourceLocation 1 2 3 4)
