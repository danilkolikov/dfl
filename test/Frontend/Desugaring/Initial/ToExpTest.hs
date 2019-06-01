{- |
Module      :  Frontend.Desugaring.Initial.ToExpTest
Description :  Tests for desugaring of object to Exp-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to Exp-s
-}
module Frontend.Desugaring.Initial.ToExpTest
    ( testSuite
    ) where

import Test.Hspec

import qualified Data.List.NonEmpty as NE

import qualified Frontend.Desugaring.Initial.Ast as D
    ( Const(..)
    , Exp(..)
    , Ident(..)
    )
import Frontend.Desugaring.Initial.ToExp (desugarToExp)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (withDummyLocation)
import Frontend.Syntax.Token

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToExp" $ do
        it "desugars AExp" $ do
            let qVar1 =
                    withDummyLocation . FuncLabelId . Qualified [] . VarId $ "a"
                qVar1Expected = withDummyLocation $ D.IdentNamed ["a"]
                gCon1 = withDummyLocation GConUnit
                gCon1Expected = withDummyLocation $ D.IdentNamed ["()"]
                lit1 =
                    withDummyLocation .
                    LiteralInteger . withDummyLocation . IntT $
                    42
                lit1Expected = withDummyLocation $ D.ConstInt 42
                aExp1 = withDummyLocation $ AExpVariable qVar1
                aExp1Expected = withDummyLocation $ D.ExpVar qVar1Expected
                aExp2 = withDummyLocation $ AExpConstructor gCon1
                aExp2Expected = withDummyLocation $ D.ExpConstr gCon1Expected
                aExp3 = withDummyLocation $ AExpLiteral lit1
                aExp3Expected = withDummyLocation $ D.ExpConst lit1Expected
                aExp4 = withDummyLocation $ AExpParens exp1
                aExp4Expected = exp1Expected
                aExp5 = withDummyLocation $ AExpTuple exp1 exp2 []
                aExp5Expected =
                    withDummyLocation $
                    D.ExpApplication
                        (withDummyLocation . D.ExpVar . withDummyLocation $
                         D.IdentParametrised ["(,)"] 2)
                        (exp1Expected NE.:| [exp2Expected])
                aExp6 = withDummyLocation $ AExpList (exp1 NE.:| [])
                aExp6Expected =
                    withDummyLocation
                        (D.ExpApplication
                             (withDummyLocation .
                              D.ExpVar . withDummyLocation . D.IdentNamed $
                              [":"])
                             (exp1Expected NE.:|
                              [ withDummyLocation .
                                D.ExpConstr . withDummyLocation . D.IdentNamed $
                                ["[]"]
                              ]))
                aExp7 =
                    withDummyLocation $
                    AExpSequence exp1 (Just exp2) (Just exp2)
                aExp7Expected =
                    withDummyLocation $
                    D.ExpApplication
                        (withDummyLocation .
                         D.ExpVar . withDummyLocation . D.IdentNamed $
                         ["enumFromThenTo"])
                        (exp1Expected NE.:| [exp2Expected, exp2Expected])
                exp1 =
                    withDummyLocation .
                    ExpSimple .
                    withDummyLocation .
                    InfixExpLExp .
                    withDummyLocation . (\x -> LExpApplication (x NE.:| [])) $
                    aExp2
                exp1Expected = aExp2Expected
                exp2 =
                    withDummyLocation .
                    ExpSimple .
                    withDummyLocation .
                    InfixExpLExp .
                    withDummyLocation . (\x -> LExpApplication (x NE.:| [])) $
                    aExp3
                exp2Expected = aExp3Expected
            desugarToExp aExp1 `shouldBe` aExp1Expected
            desugarToExp aExp2 `shouldBe` aExp2Expected
            desugarToExp aExp3 `shouldBe` aExp3Expected
            desugarToExp aExp4 `shouldBe` aExp4Expected
            desugarToExp aExp5 `shouldBe` aExp5Expected
            desugarToExp aExp6 `shouldBe` aExp6Expected
            desugarToExp aExp7 `shouldBe` aExp7Expected
            -- TODO: add more tests
