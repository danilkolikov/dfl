{- |
Module      :  Frontend.Desugaring.Final.ExpressionDesugaringCaseTest
Description :  Tests for desugaring of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of case expressions
-}
module Frontend.Desugaring.Final.ExpressionDesugaringCaseTest
    ( testSuite
    ) where

import Test.Hspec

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Final.Ast hiding (getDataTypeConstructors)
import Frontend.Desugaring.Final.ExpressionDesugaringBase
import Frontend.Desugaring.Final.ExpressionDesugaringCase
import qualified Frontend.Desugaring.Final.ResolvedAst as R
import Frontend.Desugaring.Final.Utils
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

testSuite :: IO ()
testSuite =
    hspec $ do
        let makeGeneratedIdent =
                withDummyLocation .
                IdentGenerated IdentEnvironmentExpressionDesugaring
        describe "abstractExpAway" $
            it "abstracts expressions away" $ do
                let func = return . withDummyLocation . ExpVar
                    arg = makeExp ["a"]
                    newIdent = makeGeneratedIdent 0
                    exp' = withDummyLocation $ ExpVar newIdent
                    abstraction =
                        withDummyLocation $ ExpAbstraction newIdent exp'
                    application =
                        withDummyLocation $
                        ExpApplication abstraction (arg NE.:| [])
                runExpressionDesugaringState (abstractExpAway func arg) 0 `shouldBe`
                    (application, 1)
        describe "wrapToLet" $ do
            let exp' = makeExp ["x"]
            it "doesn't add let when decls are empty" $
                wrapToLet HM.empty exp' `shouldBe` getValue exp'
            it "adds let when decls are not empty" $ do
                let name = IdentNamed ["a"]
                    name' = withDummyLocation name
                    expression = Expression name' (makeExp ["x"]) Nothing
                    nonEmpty = HM.singleton name expression
                    expected = ExpLet nonEmpty exp'
                wrapToLet nonEmpty exp' `shouldBe` expected
        describe "desugarPattern" $ do
            let caseIdent = withDummyLocation $ IdentNamed ["case"]
                elseIdent = withDummyLocation $ IdentNamed ["fail"]
                success = makeExp ["success"]
                desugarPattern' = desugarPattern caseIdent elseIdent success
            it "desugars vars" $ do
                let var1 = withDummyLocation $ IdentNamed ["var1"]
                    pattern1 = withDummyLocation $ R.PatternVar var1 Nothing
                    caseVar = withDummyLocation (ExpVar caseIdent)
                    exp1 =
                        withDummyLocation $
                        ExpApplication
                            (withDummyLocation $ ExpAbstraction var1 success)
                            (caseVar NE.:| [])
                runExpressionDesugaringState (desugarPattern' pattern1) 0 `shouldBe`
                    (exp1, 0)
                let var2 = withDummyLocation $ IdentNamed ["var2"]
                    pattern2 =
                        withDummyLocation $ R.PatternVar var2 (Just pattern1)
                    var2Inner =
                        withDummyLocation $
                        ExpApplication
                            (withDummyLocation $ ExpAbstraction var2 success)
                            (caseVar NE.:| [])
                    exp2 =
                        withDummyLocation $
                        ExpApplication
                            (withDummyLocation $ ExpAbstraction var1 var2Inner)
                            (caseVar NE.:| [])
                runExpressionDesugaringState (desugarPattern' pattern2) 0 `shouldBe`
                    (exp2, 0)
            it "desugars wildcard" $ do
                let pattern' = withDummyLocation R.PatternWildcard
                    exp' = success
                runExpressionDesugaringState (desugarPattern' pattern') 0 `shouldBe`
                    (exp', 0)
            it "desugars const" $ do
                let const' = withDummyLocation $ ConstInt 1
                    pattern' = withDummyLocation $ R.PatternConst const'
                    application =
                        withDummyLocation $
                        ExpApplication
                            (makeExp eQUAL_NAME)
                            (withDummyLocation (ExpVar caseIdent) NE.:|
                             [withDummyLocation (ExpConst const')])
                    newIdent = makeGeneratedIdent 0
                    case' =
                        withDummyLocation $
                        ExpCase newIdent trueIdent [] success elseIdent
                    abstraction =
                        withDummyLocation $ ExpAbstraction newIdent case'
                    exp' =
                        withDummyLocation $
                        ExpApplication abstraction (application NE.:| [])
                runExpressionDesugaringState (desugarPattern' pattern') 0 `shouldBe`
                    (exp', 1)
            it "desugars constructors" $ do
                let name1 = makeIdent ["Unit"]
                    pattern1 = withDummyLocation $ R.PatternConstr name1 []
                    exp1 =
                        withDummyLocation $
                        ExpCase caseIdent name1 [] success elseIdent
                runExpressionDesugaringState (desugarPattern' pattern1) 0 `shouldBe`
                    (exp1, 0)
                let name2 = makeIdent ["Just"]
                    pattern2 =
                        withDummyLocation $ R.PatternConstr name2 [pattern1]
                    newIdent = makeGeneratedIdent 0
                    exp2 =
                        withDummyLocation $
                        ExpCase
                            caseIdent
                            name2
                            [newIdent]
                            (withDummyLocation $
                             ExpCase newIdent name1 [] success elseIdent)
                            elseIdent
                runExpressionDesugaringState (desugarPattern' pattern2) 0 `shouldBe`
                    (exp2, 1)
        describe "desugarGuard" $ do
            let ifSuccess = makeExp ["success"]
                ifFail = withDummyLocation $ IdentNamed ["fail"]
                desugarGuard' = desugarGuard ifSuccess ifFail
            it "desugars pattern guard" $ do
                let name' = makeIdent ["Unit"]
                    pattern' = withDummyLocation $ R.PatternConstr name' []
                    inner = makeExp ["x"]
                    stmt =
                        withDummyLocation $ PreparedStmtPattern pattern' inner
                    ident = makeGeneratedIdent 0
                    case' =
                        withDummyLocation $
                        ExpCase ident name' [] ifSuccess ifFail
                    abstraction = withDummyLocation $ ExpAbstraction ident case'
                    application =
                        withDummyLocation $
                        ExpApplication abstraction (inner NE.:| [])
                runExpressionDesugaringState (desugarGuard' stmt) 0 `shouldBe`
                    (application, 1)
            it "desugard let guard" $ do
                let empty = HM.empty
                    stmt1 = withDummyLocation $ PreparedStmtLet empty
                runExpressionDesugaringState (desugarGuard' stmt1) 0 `shouldBe`
                    (ifSuccess, 0)
            it "desugar expression guard" $ do
                let inner = makeExp ["x"]
                    stmt = withDummyLocation $ PreparedStmtExp inner
                    ident = makeGeneratedIdent 0
                    case' =
                        withDummyLocation $
                        ExpCase ident trueIdent [] ifSuccess ifFail
                    abstraction = withDummyLocation $ ExpAbstraction ident case'
                    application =
                        withDummyLocation $
                        ExpApplication abstraction (inner NE.:| [])
                runExpressionDesugaringState (desugarGuard' stmt) 0 `shouldBe`
                    (application, 1)
        describe "desugarGuards" $ do
            let ifSuccess = makeExp ["success"]
                ifFail = withDummyLocation $ IdentNamed ["fail"]
                desugarGuards' = desugarGuards ifSuccess ifFail
                name1 = makeIdent ["Unit1"]
                pattern1 = withDummyLocation $ R.PatternConstr name1 []
                inner1 = makeExp ["x"]
                stmt1 = withDummyLocation $ PreparedStmtPattern pattern1 inner1
                name2 = makeIdent ["Unit2"]
                pattern2 = withDummyLocation $ R.PatternConstr name2 []
                inner2 = makeExp ["y"]
                stmt2 = withDummyLocation $ PreparedStmtPattern pattern2 inner2
                ident1 = makeGeneratedIdent 1
                ident2 = makeGeneratedIdent 0
                case1 =
                    withDummyLocation $
                    ExpCase ident1 name1 [] application2 ifFail
                abstraction1 = withDummyLocation $ ExpAbstraction ident1 case1
                application1 =
                    withDummyLocation $
                    ExpApplication abstraction1 (inner1 NE.:| [])
                case2 =
                    withDummyLocation $ ExpCase ident2 name2 [] ifSuccess ifFail
                abstraction2 = withDummyLocation $ ExpAbstraction ident2 case2
                application2 =
                    withDummyLocation $
                    ExpApplication abstraction2 (inner2 NE.:| [])
            it "desugars multiple guards" $
                runExpressionDesugaringState
                    (desugarGuards' (stmt1 NE.:| [stmt2]))
                    0 `shouldBe`
                (application1, 2)
        describe "desugarGuardedExps" $ do
            let exp1 = makeExp ["success1"]
                name1 = makeIdent ["Unit1"]
                pattern1 = withDummyLocation $ R.PatternConstr name1 []
                inner1 = makeExp ["x"]
                stmt1 = withDummyLocation $ PreparedStmtPattern pattern1 inner1
                guarded1 =
                    withDummyLocation $ PreparedGuardedExp (stmt1 NE.:| []) exp1
                name2 = makeIdent ["Unit2"]
                pattern2 = withDummyLocation $ R.PatternConstr name2 []
                inner2 = makeExp ["y"]
                exp2 = makeExp ["success2"]
                stmt2 = withDummyLocation $ PreparedStmtPattern pattern2 inner2
                guarded2 =
                    withDummyLocation $ PreparedGuardedExp (stmt2 NE.:| []) exp2
                ident1 = makeGeneratedIdent 3
                ident2 = makeGeneratedIdent 1
                ident3 = makeGeneratedIdent 2
                ident4 = makeGeneratedIdent 0
                case1 = withDummyLocation $ ExpCase ident1 name1 [] exp1 ident3
                abstraction1 = withDummyLocation $ ExpAbstraction ident1 case1
                application1 =
                    withDummyLocation $
                    ExpApplication abstraction1 (inner1 NE.:| [])
                case2 = withDummyLocation $ ExpCase ident2 name2 [] exp2 ident4
                abstraction2 = withDummyLocation $ ExpAbstraction ident2 case2
                application2 =
                    withDummyLocation $
                    ExpApplication abstraction2 (inner2 NE.:| [])
                abstraction3 =
                    withDummyLocation $ ExpAbstraction ident3 application1
                abstraction4 =
                    withDummyLocation $ ExpAbstraction ident4 application2
                application3 =
                    withDummyLocation $
                    ExpApplication abstraction3 (application4 NE.:| [])
                application4 =
                    withDummyLocation $
                    ExpApplication abstraction4 (undefinedExp NE.:| [])
            it "should desugar guarded expressions" $
                runExpressionDesugaringState
                    (desugarGuardedExps (guarded1 NE.:| [guarded2]))
                    0 `shouldBe`
                (application3, 4)
        describe "desugarAlt" $ do
            let caseIdent = makeIdent ["case"]
                elseIdent = makeIdent ["else"]
                desugarAlt' = desugarAlt caseIdent elseIdent
                pattern' = withDummyLocation R.PatternWildcard
                exp' = makeExp ["x"]
            it "should desugar simple alt" $ do
                let alt = withDummyLocation $ PreparedAltSimple pattern' exp'
                runExpressionDesugaringState (desugarAlt' alt) 0 `shouldBe`
                    (exp', 0)
            it "should desugar guarded alt" $ do
                let stmt = withDummyLocation $ PreparedStmtLet HM.empty
                    guardedExp =
                        withDummyLocation $
                        PreparedGuardedExp (stmt NE.:| []) exp'
                    alt1 =
                        withDummyLocation $
                        PreparedAltGuarded
                            pattern'
                            (guardedExp NE.:| [])
                            HM.empty
                    ident1 = makeGeneratedIdent 0
                    abstraction1 =
                        withDummyLocation $ ExpAbstraction ident1 exp'
                    application1 =
                        withDummyLocation $
                        ExpApplication abstraction1 (undefinedExp NE.:| [])
                runExpressionDesugaringState (desugarAlt' alt1) 0 `shouldBe`
                    (application1, 1)
        describe "desugarPatternsToAbstraction'" $ do
            let pattern1 = makeRPattern ["True"]
                pattern2 = makeRPattern ["True"]
                pattern3 = withDummyLocation R.PatternWildcard
                pattern4 = withDummyLocation R.PatternWildcard
                exp1 = makeExp ["x"]
                exp2 = makeExp ["y"]
                arg1 = (pattern1 NE.:| [pattern2], exp1)
                arg2 = (pattern3 NE.:| [pattern4], exp2)
                args = arg1 NE.:| [arg2]
                ident0 = makeGeneratedIdent 0
                ident1 = makeGeneratedIdent 1
                ident2 = makeGeneratedIdent 2
                ident3 = makeGeneratedIdent 3
                ident4 = makeGeneratedIdent 4
                ident5 = makeGeneratedIdent 5
                ident6 = makeGeneratedIdent 6
                ident7 = makeGeneratedIdent 7
                ident8 = makeGeneratedIdent 8
                tuple = withDummyLocation $ IdentParametrised ["(,)"] 2
                true = makeIdent ["True"]
                case1inner =
                    withDummyLocation $ ExpCase ident8 true [] exp1 ident6
                case1 =
                    withDummyLocation $ ExpCase ident7 true [] case1inner ident6
                tupleCase1 =
                    withDummyLocation $
                    ExpCase ident2 tuple [ident7, ident8] case1 ident6
                abstraction1 =
                    withDummyLocation $ ExpAbstraction ident6 tupleCase1
                tupleCase2 =
                    withDummyLocation $
                    ExpCase ident2 tuple [ident4, ident5] exp2 ident3
                abstraction2 =
                    withDummyLocation $ ExpAbstraction ident3 tupleCase2
                application2 =
                    withDummyLocation $
                    ExpApplication abstraction2 (undefinedExp NE.:| [])
                application1 =
                    withDummyLocation $
                    ExpApplication abstraction1 (application2 NE.:| [])
                caseAbstraction =
                    withDummyLocation $ ExpAbstraction ident2 application1
                tupleExp =
                    withDummyLocation $
                    ExpApplication
                        (withDummyLocation $ ExpVar tuple)
                        (withDummyLocation (ExpVar ident0) NE.:|
                         [withDummyLocation $ ExpVar ident1])
                tupleApplication =
                    withDummyLocation $
                    ExpApplication caseAbstraction (tupleExp NE.:| [])
                res =
                    ExpAbstraction
                        ident0
                        (withDummyLocation $
                         ExpAbstraction ident1 tupleApplication)
            it "desugars patterns to abstraction" $
                runExpressionDesugaringState
                    (desugarPatternsToAbstraction' 2 args)
                    0 `shouldBe`
                (res, 9)
