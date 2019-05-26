{- |
Module      :  Frontend.Desugaring.ToSimpleClassTest
Description :  Tests for desugaring of object to SimpleClass-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to SimpleClass-s
-}
module Frontend.Desugaring.ToSimpleClassTest
    ( testSuite
    ) where

import Test.Hspec

import qualified Frontend.Desugaring.Ast as D
import Frontend.Desugaring.ToSimpleClass (desugarToSimpleClass)
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
    describe "desugarToSimpleClass" $ do
        it "should desugar SimpleClass" $
            desugarToSimpleClass
                (SimpleClass
                     (withDummyLocation (Qualified [] (ConId "Class")))
                     (withDummyLocation (VarId "a"))) `shouldBe`
            withDummyLocation
                (D.SimpleClass
                     (withDummyLocation (D.IdentNamed ["Class"]))
                     (withDummyLocation (D.IdentNamed ["a"])))
        it "keeps track of locations" $
            desugarToSimpleClass
                (WithLocation
                     (SimpleClass
                          (withDummyLocation (Qualified [] (ConId "Class")))
                          (withDummyLocation (VarId "a")))
                     (sourceLocation 1 2 3 4)) `shouldBe`
            WithLocation
                (D.SimpleClass
                     (withDummyLocation (D.IdentNamed ["Class"]))
                     (withDummyLocation (D.IdentNamed ["a"])))
                (sourceLocation 1 2 3 4)
