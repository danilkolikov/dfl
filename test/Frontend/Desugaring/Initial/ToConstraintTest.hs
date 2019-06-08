{- |
Module      :  Frontend.Desugaring.Initial.ToConstraintTest
Description :  Tests for desugaring of object to Constraint-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to Constraint-s
-}
module Frontend.Desugaring.Initial.ToConstraintTest
    ( testSuite
    ) where

import Test.Hspec

import qualified Data.List.NonEmpty as NE

import qualified Frontend.Desugaring.Initial.Ast as D
    ( Constraint(..)
    , Ident(..)
    , Type(..)
    )
import Frontend.Desugaring.Initial.ToConstraint (desugarToConstraint)
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
    describe "desugarToConstraint" $ do
        let className = Qualified [] $ ConId "Class"
            classObj = WithLocation className (sourceLocation 1 2 3 4)
            classExpected =
                WithLocation (D.IdentNamed ["Class"]) (sourceLocation 1 2 3 4)
            varName = VarId "a"
            varObj = WithLocation varName (sourceLocation 5 6 7 8)
            varExpected =
                WithLocation (D.IdentNamed ["a"]) (sourceLocation 5 6 7 8)
            aType =
                withDummyLocation . ATypeVar . withDummyLocation . VarId $ "b"
            typeExpected =
                withDummyLocation . D.TypeVar . withDummyLocation . D.IdentNamed $
                ["b"]
        it "should desugar constraints" $ do
            desugarToConstraint (withDummyLocation $ ClassSimple classObj varObj) `shouldBe`
                    withDummyLocation (D.Constraint classExpected varExpected [])
            desugarToConstraint
                    (withDummyLocation $ ClassApplied classObj varObj (aType NE.:| [])) `shouldBe`
                    withDummyLocation (D.Constraint classExpected varExpected [typeExpected])
        it "should keep track of locations" $
            desugarToConstraint
                (WithLocation
                     (ClassSimple classObj varObj)
                     (sourceLocation 1 2 3 4)) `shouldBe`
            WithLocation
                (D.Constraint classExpected varExpected [])
                (sourceLocation 1 2 3 4)
