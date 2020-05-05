{- |
Module      :  Frontend.Desugaring.Final.StmtTest
Description :  Tests for desugaring of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of statements
-}
module Frontend.Desugaring.Final.StmtTest
    ( testSuite
    ) where

import Test.Hspec

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Core.PredefinedIdents
import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.Base
import Frontend.Desugaring.Final.Statement
import qualified Frontend.Desugaring.Final.Util as U
import qualified Frontend.Desugaring.Record.Ast as R
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

testSuite :: IO ()
testSuite =
    hspec $ do
        let makeGeneratedIdent =
                withDummyLocation .
                IdentGenerated .
                GeneratedIdent GeneratedIdentEnvironmentExpressionDesugaring
            makeIdent =
                withDummyLocation . IdentUserDefined . IdentSimple . IdentNamed
            makeExp = withDummyLocation . ExpVar . makeIdent
            exp' = makeExp "x"
        describe "desugarDoStmt" $ do
            let desugarDoStmt' = (`desugarDoStmt` exp')
            it "desugars single expressions" $ do
                let inner = makeExp "y"
                    preparedStmt = withDummyLocation $ PreparedStmtExp inner
                    func = U.makeExp iGNORING_BIND
                    expected = ExpApplication func (inner NE.:| [exp'])
                runExpressionDesugaringProcessor (desugarDoStmt' preparedStmt) 0 `shouldBe`
                    expected
            it "desugars let bindings" $ do
                let preparedStmt = withDummyLocation $ PreparedStmtLet HM.empty
                    expected = getValue exp'
                runExpressionDesugaringProcessor (desugarDoStmt' preparedStmt) 0 `shouldBe`
                    expected
            it "desugards pattern bindings" $ do
                let pattern' =
                        withDummyLocation $ R.PatternConstr U.trueIdent []
                    inner = makeExp "y"
                    stmt =
                        withDummyLocation $ PreparedStmtPattern pattern' inner
                    failExp = U.makeExp fAIL
                    failArg =
                        withDummyLocation .
                        ExpConst . withDummyLocation . ConstString $
                        "Pattern not matched"
                    failApplication =
                        withDummyLocation $
                        ExpApplication failExp (failArg NE.:| [])
                    ident0 = makeGeneratedIdent 0
                    ident1 = makeGeneratedIdent 1
                    ident2 = makeGeneratedIdent 2
                    ident3 = makeGeneratedIdent 3
                    case' =
                        withDummyLocation $
                        ExpCase ident1 U.trueIdent [] exp' ident3
                    abstraction3 =
                        withDummyLocation $ ExpAbstraction ident3 case'
                    abstraction2 =
                        withDummyLocation $
                        ExpAbstraction ident2 failApplication
                    application2 =
                        withDummyLocation $
                        ExpApplication abstraction2 (U.undefinedExp NE.:| [])
                    application3 =
                        withDummyLocation $
                        ExpApplication abstraction3 (application2 NE.:| [])
                    abstraction1 =
                        withDummyLocation $ ExpAbstraction ident1 application3
                    expression = Expression ident0 abstraction1 Nothing Nothing
                    letDecls = HM.singleton (getValue ident0) expression
                    bindExp = U.makeExp bIND
                    rhs =
                        withDummyLocation $
                        ExpApplication
                            bindExp
                            (inner NE.:| [withDummyLocation (ExpVar ident0)])
                    expected = ExpLet letDecls rhs
                runExpressionDesugaringProcessor (desugarDoStmt' stmt) 0 `shouldBe`
                    expected
        describe "desugarListComprehensionStmt" $ do
            let desugarListComprehensionStmt' =
                    desugarListComprehensionStmt exp'
            it "desugars single expressions" $ do
                let inner = makeExp "y"
                    preparedStmt = withDummyLocation $ PreparedStmtExp inner
                    ident0 = makeGeneratedIdent 0
                    ident1 = makeGeneratedIdent 1
                    ident2 = makeGeneratedIdent 2
                    case' =
                        withDummyLocation $
                        ExpCase ident0 U.trueIdent [] exp' ident2
                    abstraction2 =
                        withDummyLocation $ ExpAbstraction ident2 case'
                    application2 =
                        withDummyLocation $
                        ExpApplication abstraction2 (application1 NE.:| [])
                    abstraction1 =
                        withDummyLocation $ ExpAbstraction ident1 U.emptyList
                    application1 =
                        withDummyLocation $
                        ExpApplication abstraction1 (U.undefinedExp NE.:| [])
                    abstraction0 =
                        withDummyLocation $ ExpAbstraction ident0 application2
                    application0 = ExpApplication abstraction0 (inner NE.:| [])
                runExpressionDesugaringProcessor
                    (desugarListComprehensionStmt' preparedStmt)
                    0 `shouldBe`
                    application0
            it "desugars let bindings" $ do
                let preparedStmt = withDummyLocation $ PreparedStmtLet HM.empty
                    expected = getValue exp'
                runExpressionDesugaringProcessor
                    (desugarListComprehensionStmt' preparedStmt)
                    0 `shouldBe`
                    expected
            it "desugards pattern bindings" $ do
                let pattern' =
                        withDummyLocation $ R.PatternConstr U.trueIdent []
                    inner = makeExp "y"
                    stmt =
                        withDummyLocation $ PreparedStmtPattern pattern' inner
                    ident0 = makeGeneratedIdent 0
                    ident1 = makeGeneratedIdent 1
                    ident2 = makeGeneratedIdent 2
                    ident3 = makeGeneratedIdent 3
                    case' =
                        withDummyLocation $
                        ExpCase ident1 U.trueIdent [] exp' ident3
                    abstraction3 =
                        withDummyLocation $ ExpAbstraction ident3 case'
                    abstraction2 =
                        withDummyLocation $ ExpAbstraction ident2 U.emptyList
                    application2 =
                        withDummyLocation $
                        ExpApplication abstraction2 (U.undefinedExp NE.:| [])
                    application3 =
                        withDummyLocation $
                        ExpApplication abstraction3 (application2 NE.:| [])
                    abstraction1 =
                        withDummyLocation $ ExpAbstraction ident1 application3
                    expression = Expression ident0 abstraction1 Nothing Nothing
                    letDecls = HM.singleton (getValue ident0) expression
                    func = U.makeExp cONCAT_MAP
                    rhs =
                        withDummyLocation $
                        ExpApplication
                            func
                            (withDummyLocation (ExpVar ident0) NE.:| [inner])
                    expected = ExpLet letDecls rhs
                runExpressionDesugaringProcessor
                    (desugarListComprehensionStmt' stmt)
                    0 `shouldBe`
                    expected
