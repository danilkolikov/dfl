{- |
Module      :  Frontend.Desugaring.Initial.ToIdentTest
Description :  Tests for desugaring of object to Ident-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to Ident-s
-}
module Frontend.Desugaring.Initial.ToIdentTest
    ( testSuite
    ) where

import Test.Hspec

import Frontend.Desugaring.Initial.Ast (Ident(..))
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
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
    describe "desugarToIdent" $ do
        it "should desugar simple names" $ do
            desugarToIdent (VarId "id") `shouldBe`
                withDummyLocation (IdentNamed ["id"])
            desugarToIdent (ConId "Con") `shouldBe`
                withDummyLocation (IdentNamed ["Con"])
            desugarToIdent (VarSym "+") `shouldBe`
                withDummyLocation (IdentNamed ["+"])
            desugarToIdent (ConSym ":|") `shouldBe`
                withDummyLocation (IdentNamed [":|"])
        it "should desugar qualified IDs" $
            desugarToIdent
                (Qualified [ConId "Module", ConId "Nested"] (VarId "id")) `shouldBe`
            withDummyLocation (IdentNamed ["Module", "Nested", "id"])
        it "should desugar GCon" $ do
            desugarToIdent GConList `shouldBe`
                withDummyLocation (IdentNamed ["[]"])
            desugarToIdent GConUnit `shouldBe`
                withDummyLocation (IdentNamed ["()"])
            desugarToIdent (GConTuple 5) `shouldBe`
                withDummyLocation (IdentParametrised ["(,)"] 5)
        it "should desugar GTyCon" $ do
            desugarToIdent GTyConList `shouldBe`
                withDummyLocation (IdentNamed ["[]"])
            desugarToIdent GTyConUnit `shouldBe`
                withDummyLocation (IdentNamed ["()"])
            desugarToIdent (GTyConTuple 5) `shouldBe`
                withDummyLocation (IdentParametrised ["(,)"] 5)
            desugarToIdent GTyConFunction `shouldBe`
                withDummyLocation (IdentNamed ["->"])
        it "should keep track of locations" $
            desugarToIdent (WithLocation GTyConList (sourceLocation 1 2 3 4)) `shouldBe`
            WithLocation (IdentNamed ["[]"]) (sourceLocation 1 2 3 4)