{- |
Module      :  Frontend.Desugaring.Final.ExpressionDesugaringAssignmentTest
Description :  Tests for desugaring of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of assignments
-}
module Frontend.Desugaring.Final.ExpressionDesugaringAssignmentTest
    ( testSuite
    ) where

import Test.Hspec

import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Final.Ast hiding (getDataTypeConstructors)
import Frontend.Desugaring.Final.ExpressionDesugaringAssignment
import Frontend.Desugaring.Final.ExpressionDesugaringBase
import qualified Frontend.Desugaring.Final.ResolvedAst as R
import Frontend.Desugaring.Final.Utils
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

testSuite :: IO ()
testSuite =
    hspec $ do
        let makeGeneratedIdent =
                withDummyLocation .
                IdentGenerated IdentEnvironmentExpressionDesugaring
            wildcard = withDummyLocation R.PatternWildcard
            literal =
                withDummyLocation .
                R.PatternConst . withDummyLocation . ConstInt $
                42
            yIdent = makeIdent ["y"]
            yVar = withDummyLocation $ R.PatternVar yIdent Nothing
            zIdent = makeIdent ["z"]
            zVar = withDummyLocation $ R.PatternVar zIdent Nothing
            aIdent = makeIdent ["A"]
            inner =
                withDummyLocation $
                R.PatternConstr aIdent [wildcard, literal, yVar, zVar]
            xIdent = makeIdent ["x"]
            var = withDummyLocation $ R.PatternVar xIdent (Just inner)
            innerWithoutVars =
                withDummyLocation $
                R.PatternConstr aIdent [wildcard, literal, wildcard, wildcard]
        describe "patternWithoutVariables" $
            it "removes variables from a pattern" $ do
                let varExpected = innerWithoutVars
                patternWithoutVariables var `shouldBe` varExpected
        describe "getVariablesFromPattern" $
            it "selects variables from a pattern" $ do
                let xExpected =
                        ( withDummyLocation $
                          R.PatternVar xIdent (Just innerWithoutVars)
                        , Just xIdent)
                    yExpected =
                        ( withDummyLocation $
                          R.PatternConstr
                              aIdent
                              [wildcard, literal, yVar, wildcard]
                        , Just yIdent)
                    zExpected =
                        ( withDummyLocation $
                          R.PatternConstr
                              aIdent
                              [wildcard, literal, wildcard, zVar]
                        , Just zIdent)
                getVariablesFromPattern var `shouldBe`
                    [xExpected, yExpected, zExpected]
        let exp' = makeExp ["x"]
        describe "processPattern" $
            it "Binds a single variable from a pattern" $ do
                let pat = yVar
                    ident0 = makeGeneratedIdent 0
                    ident1 = makeGeneratedIdent 1
                    abstractionY =
                        withDummyLocation $
                        ExpAbstraction
                            yIdent
                            (withDummyLocation $ ExpVar yIdent)
                    applicationY =
                        withDummyLocation $
                        ExpApplication
                            abstractionY
                            (withDummyLocation (ExpVar ident0) NE.:| [])
                    abstraction1 =
                        withDummyLocation $ ExpAbstraction ident1 applicationY
                    application1 =
                        withDummyLocation $
                        ExpApplication abstraction1 (undefinedExp NE.:| [])
                    abstraction0 =
                        withDummyLocation $ ExpAbstraction ident0 application1
                    application0 =
                        withDummyLocation $
                        ExpApplication abstraction0 (exp' NE.:| [])
                    expression = Expression yIdent application0 Nothing
                runExpressionDesugaringProcessor
                    (processPattern exp' (pat, Just yIdent))
                    0 `shouldBe`
                    (Right $ Just (getValue yIdent, expression), 2)
        describe "desugarSinglePattern" $
            it "doesn't split expression if the pattern is a variable binding" $ do
                let pat = yVar
                    expression = Expression yIdent exp' Nothing
                runExpressionDesugaringProcessor
                    (desugarSinglePattern (pat, exp'))
                    0 `shouldBe`
                    (Right [(getValue yIdent, expression)], 0)
