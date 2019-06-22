{- |
Module      :  Frontend.Desugaring.Final.TypeSynonymDesugaringTest
Description :  Tests for desugaring of type synonyms
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of type synonyms
-}
module Frontend.Desugaring.Final.TypeSynonymDesugaringTest
    ( testSuite
    ) where

import Test.Hspec

import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.Processor
import Frontend.Desugaring.Final.TypeSynonymDesugaring
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (withDummyLocation)

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarTypeSynonym" $ do
        it "ignores all declarations, except for type synonyms" $
            runDesugaringProcessor
                (desugarTypeSynonym (I.TopDeclAssignment undefined))
                emptyDesugaringState `shouldBe`
            Right (Nothing, emptyDesugaringState)
        it "desugars type synonyms" $ do
            let typeName = IdentNamed ["Type"]
                typeName' = withDummyLocation typeName
                typeArgs = [withDummyLocation $ IdentNamed ["a"]]
                type' =
                    withDummyLocation $
                    TypeConstr (withDummyLocation $ IdentNamed ["A"])
                simpleType = withDummyLocation $ I.SimpleType typeName' typeArgs
                topDecl = I.TopDeclType simpleType type'
                res = TypeSynonym typeName' typeArgs type'
            runDesugaringProcessor
                (desugarTypeSynonym topDecl)
                emptyDesugaringState `shouldBe`
                Right
                    ( Just (typeName, res)
                    , emptyDesugaringState
                          { getDefinedTypeNames =
                                HM.singleton typeName typeName'
                          })
