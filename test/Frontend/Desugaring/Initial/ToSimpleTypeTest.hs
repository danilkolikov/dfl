{- |
Module      :  Frontend.Desugaring.Initial.ToSimpleTypeTest
Description :  Tests for desugaring of object to SimpleType-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to SimpleType-s
-}
module Frontend.Desugaring.Initial.ToSimpleTypeTest
    ( testSuite
    ) where

import Test.Hspec

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToSimpleType (desugarToSimpleType)
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
    describe "desugarToSimpleType" $ do
        it "should desugar SimpleType" $
            desugarToSimpleType
                (withDummyLocation $ SimpleType
                     (withDummyLocation (ConId "Type"))
                     [withDummyLocation (VarId "a")]) `shouldBe`
            withDummyLocation
                (D.SimpleType
                     (withDummyLocation (D.IdentNamed ["Type"]))
                     [withDummyLocation (D.IdentNamed ["a"])])
        it "keeps track of locations" $
            desugarToSimpleType
                (WithLocation
                     (SimpleType
                          (withDummyLocation (ConId "Type"))
                          [withDummyLocation (VarId "a")])
                     (sourceLocation 1 2 3 4)) `shouldBe`
            WithLocation
                (D.SimpleType
                     (withDummyLocation (D.IdentNamed ["Type"]))
                     [withDummyLocation (D.IdentNamed ["a"])])
                (sourceLocation 1 2 3 4)
