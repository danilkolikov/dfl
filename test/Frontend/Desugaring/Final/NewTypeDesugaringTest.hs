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
        let typeName = IdentNamed ["Type"]
            typeName' = withDummyLocation typeName
            typeArgs = [withDummyLocation $ IdentNamed ["a"]]
            type' =
                withDummyLocation $
                TypeConstr (withDummyLocation $ IdentNamed ["A"])
            simpleType = withDummyLocation $ I.SimpleType typeName' typeArgs
        it "desugars simple newtypes" $ do
            let newConstr =
                    withDummyLocation $ I.NewConstrSimple typeName' type'
                topDecl = I.TopDeclNewType [] simpleType newConstr []
                resConstr = (typeName, Constructor typeName' [type'] HM.empty)
                res = NewType [] typeName' typeArgs [] resConstr
            runDesugaringProcessor (desugarNewType topDecl) emptyDesugaringState `shouldBe`
                Right
                    ( Just (typeName, res)
                    , emptyDesugaringState
                          { getDefinedTypeNames =
                                HM.singleton typeName typeName'
                          })
        it "desugars record-style newtypes" $ do
            let getter = IdentNamed ["getType"]
                getter' = withDummyLocation getter
                newConstr =
                    withDummyLocation $
                    I.NewConstrRecord typeName' getter' type'
                topDecl = I.TopDeclNewType [] simpleType newConstr []
                resConstr =
                    ( typeName
                    , Constructor typeName' [type'] (HM.singleton getter 0))
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
