{- |
Module      :  Frontend.Desugaring.Initial.ToConstrTest
Description :  Tests for desugaring of object to Constr-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to Constr-s
-}
module Frontend.Desugaring.Initial.ToConstrTest
    ( testSuite
    ) where

import Test.Hspec

import qualified Data.List.NonEmpty as NE

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToConstr (desugarToConstr)
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
    describe "desugarToConstr" $ do
        let aType1 =
                withDummyLocation . ATypeVar . withDummyLocation . VarId $ "a"
            bType1 = withDummyLocation $ BType (aType1 NE.:| [])
            type1 = withDummyLocation $ Type (bType1 NE.:| [])
            type1Expected =
                withDummyLocation . D.TypeVar . withDummyLocation . D.IdentNamed $
                ["a"]
            aType2 =
                withDummyLocation . ATypeVar . withDummyLocation . VarId $ "b"
            bType2 = withDummyLocation $ BType (aType2 NE.:| [])
            type2Expected =
                withDummyLocation . D.TypeVar . withDummyLocation . D.IdentNamed $
                ["b"]
            conName = withDummyLocation (FuncLabelId (ConId "Type"))
            conNameExpected = withDummyLocation (D.IdentNamed ["Type"])
            conOp = withDummyLocation (OpLabelSym (ConSym ":|"))
            conOpExpected = withDummyLocation (D.IdentNamed [":|"])
            var1 = withDummyLocation $ FuncLabelId $ VarId "foo"
            var1Expected = withDummyLocation $ D.IdentNamed ["foo"]
            var2 = withDummyLocation $ FuncLabelId $ VarId "bar"
            var2Expected = withDummyLocation $ D.IdentNamed ["bar"]
        it "should desugar Constr" $ do
            desugarToConstr (ConstrSimple conName [aType1]) `shouldBe`
                withDummyLocation
                    (D.ConstrSimple conNameExpected [type1Expected])
            desugarToConstr (ConstrInfix bType1 conOp bType2) `shouldBe`
                withDummyLocation
                    (D.ConstrSimple conOpExpected [type1Expected, type2Expected])
            desugarToConstr
                (ConstrRecord
                     conName
                     [withDummyLocation $ FieldDecl (var1 NE.:| [var2]) type1]) `shouldBe`
                withDummyLocation
                    (D.ConstrRecord
                         conNameExpected
                         [ withDummyLocation $ D.FieldDecl var1Expected type1Expected
                         , withDummyLocation $ D.FieldDecl var2Expected type1Expected
                         ])
        it "keeps track of locations" $
            desugarToConstr
                (WithLocation
                     (ConstrSimple conName [aType1])
                     (sourceLocation 1 2 3 4)) `shouldBe`
            WithLocation
                (D.ConstrSimple conNameExpected [type1Expected])
                (sourceLocation 1 2 3 4)
