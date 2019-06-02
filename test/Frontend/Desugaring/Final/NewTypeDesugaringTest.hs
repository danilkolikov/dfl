{- |
Module      :  Frontend.Desugaring.Final.NewTypeDesugaringTest
Description :  Tests for desugaring of newtype-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of newtype-s
-}
module Frontend.Desugaring.Final.NewTypeDesugaringTest
    ( testSuite
    ) where

import Test.Hspec

import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.NewTypeDesugaring
import Frontend.Desugaring.Final.Processor
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (withDummyLocation)

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarNewType" $ do
        it "ignores all definitions, except for newtype" $
            runDesugaringProcessor
                (desugarNewType (I.TopDeclAssignment undefined))
                emptyDesugaringState `shouldBe`
            Right (Nothing, emptyDesugaringState)
        it "desugars newtypes" $ do
            let typeName = IdentNamed ["Type"]
                typeName' = withDummyLocation typeName
                typeArgs = [withDummyLocation $ IdentNamed ["a"]]
                type' =
                    withDummyLocation $
                    TypeConstr (withDummyLocation $ IdentNamed ["A"])
                getter = IdentNamed ["getType"]
                getter' = withDummyLocation getter
                simpleType = withDummyLocation $ I.SimpleType typeName' typeArgs
                newConstr =
                    withDummyLocation $
                    I.NewConstrRecord typeName' getter' type'
                topDecl = I.TopDeclNewType [] simpleType newConstr []
                resConstr =
                    Constructor typeName' [type'] (HM.singleton getter 0)
                res = NewType [] typeName' typeArgs [] resConstr
            runDesugaringProcessor (desugarNewType topDecl) emptyDesugaringState `shouldBe`
                Right
                    ( Just (typeName, res)
                    , emptyDesugaringState
                          { getDefinedTypeNames =
                                HM.singleton typeName typeName'
                          , getDefinedFunctionNames =
                                HM.singleton getter getter'
                          , getNewTypeFields = HM.singleton getter res
                          })
