{- |
Module      :  Frontend.Desugaring.Initial.ToNewConstrTest
Description :  Tests for desugaring of object to NewConstr-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to NewConstr-s
-}
module Frontend.Desugaring.Initial.ToNewConstrTest
    ( testSuite
    ) where

import Test.Hspec

import qualified Data.List.NonEmpty as NE

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToNewConstr (desugarToNewConstr)
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
    describe "desugarToNewConstr" $ do
        it "should desugar NewConstr" $ do
            desugarToNewConstr
                (NewConstrSimple
                     (withDummyLocation (FuncLabelId (ConId "Class")))
                     (withDummyLocation
                          (ATypeVar (withDummyLocation $ VarId "a")))) `shouldBe`
                withDummyLocation
                    (D.NewConstrSimple
                         (withDummyLocation (D.IdentNamed ["Class"]))
                         (withDummyLocation .
                          D.TypeVar . withDummyLocation . D.IdentNamed $
                          ["a"]))
            let aType =
                    withDummyLocation . ATypeVar . withDummyLocation . VarId $
                    "a"
                bType = withDummyLocation $ BType (aType NE.:| [])
                type' = withDummyLocation $ Type (bType NE.:| [])
            desugarToNewConstr
                (NewConstrNamed
                     (withDummyLocation (FuncLabelId (ConId "Class")))
                     (withDummyLocation (FuncLabelId (VarId "foo")))
                     type') `shouldBe`
                withDummyLocation
                    ((D.NewConstrNamed
                          (withDummyLocation (D.IdentNamed ["Class"]))
                          (withDummyLocation (D.IdentNamed ["foo"]))
                          (withDummyLocation
                               (D.TypeVar
                                    (withDummyLocation (D.IdentNamed ["a"]))))))
        it "keeps track of locations" $
            desugarToNewConstr
                (WithLocation
                     (NewConstrSimple
                          (withDummyLocation (FuncLabelId (ConId "Class")))
                          (withDummyLocation
                               (ATypeVar (withDummyLocation $ VarId "a"))))
                     (sourceLocation 1 2 3 4)) `shouldBe`
            WithLocation
                (D.NewConstrSimple
                     (withDummyLocation (D.IdentNamed ["Class"]))
                     (withDummyLocation .
                      D.TypeVar . withDummyLocation . D.IdentNamed $
                      ["a"]))
                (sourceLocation 1 2 3 4)
