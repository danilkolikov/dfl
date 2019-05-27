{- |
Module      :  Frontend.Desugaring.Initial.ToPatternTest
Description :  Tests for desugaring of object to Pattern-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to Pattern-s
-}
module Frontend.Desugaring.Initial.ToPatternTest
    ( testSuite
    ) where

import Test.Hspec

import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Initial.Ast
    ( Const(..)
    , Ident(..)
    , Pattern(..)
    , PatternBinding(..)
    )
import Frontend.Desugaring.Initial.ToPattern (desugarToPattern)
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
    describe "desugarToPattern" $ do
        let var1 = FuncLabelId . VarId $ "a"
            var1Expected = withDummyLocation . IdentNamed $ ["a"]
            qVar1 = FuncLabelId . Qualified [] . VarId $ "b"
            qVar1Expected = withDummyLocation . IdentNamed $ ["b"]
            qCon1 = FuncLabelId . Qualified [] . ConId $ "Con"
            qCon1Expected = withDummyLocation . IdentNamed $ ["Con"]
            qConOp = OpLabelSym GConSymColon
            qConOpExpected = withDummyLocation $ IdentNamed [":"]
            gCon1 = GConNamed (withDummyLocation qCon1)
            gCon1Expected = qCon1Expected
            lit1 = LiteralInteger . withDummyLocation . IntT $ 42
            lit1Expected = withDummyLocation . ConstInt $ 42
            aPat1 = APatVariable (withDummyLocation var1) Nothing
            aPat1Expected = withDummyLocation $ PatternVar var1Expected Nothing
            aPat2 =
                APatVariable
                    (withDummyLocation var1)
                    (Just $ withDummyLocation aPat1)
            aPat2Expected =
                withDummyLocation $ PatternVar var1Expected (Just aPat1Expected)
            aPat3 = APatConstructor (withDummyLocation GConUnit)
            aPat3Expected =
                withDummyLocation $
                PatternConstr (withDummyLocation $ IdentNamed ["()"]) []
            aPat4 =
                APatRecord
                    (withDummyLocation qCon1)
                    [ withDummyLocation $
                      FPat (withDummyLocation qVar1) (withDummyLocation pat1)
                    ]
            aPat4Expected =
                withDummyLocation $
                PatternRecord
                    qCon1Expected
                    [ withDummyLocation $
                      PatternBinding qVar1Expected pat1Expected
                    ]
            aPat5 = APatLiteral (withDummyLocation lit1)
            aPat5Expected = withDummyLocation $ PatternConst lit1Expected
            aPat6 = APatWildcard
            aPat6Expected = withDummyLocation PatternWildcard
            aPat7 = APatParens (withDummyLocation pat1)
            aPat7Expected = pat1Expected
            aPat8 =
                APatTuple (withDummyLocation pat1) (withDummyLocation pat2) []
            aPat8Expected =
                withDummyLocation $
                PatternConstr
                    (withDummyLocation $ IdentParametrised ["(,)"] 2)
                    [pat1Expected, pat2Expected]
            aPat9 = APatList (withDummyLocation pat1 NE.:| [])
            aPat9Expected =
                withDummyLocation $
                PatternConstr
                    (withDummyLocation $ IdentNamed [":"])
                    [ pat1Expected
                    , withDummyLocation $
                      PatternConstr (withDummyLocation $ IdentNamed ["[]"]) []
                    ]
            lPat1 = LPatSimple (withDummyLocation aPat1)
            lPat1Expected = aPat1Expected
            lPat2 = LPatNegated (withDummyLocation . Left . IntT $ 42)
            lPat2Expected =
                withDummyLocation $
                PatternConst (withDummyLocation $ ConstInt (-42))
            lPat3 =
                LPatConstructor
                    (withDummyLocation gCon1)
                    (withDummyLocation aPat1 NE.:| [withDummyLocation aPat2])
            lPat3Expected =
                withDummyLocation $
                PatternConstr gCon1Expected [aPat1Expected, aPat2Expected]
            pat1 = PatSimple (withDummyLocation lPat1)
            pat1Expected = lPat1Expected
            pat2 =
                PatInfix
                    (withDummyLocation pat1)
                    (withDummyLocation qConOp)
                    (withDummyLocation pat1)
            pat2Expected =
                withDummyLocation $
                PatternConstr qConOpExpected [pat1Expected, pat1Expected]
        it "should desugar APat" $ do
            desugarToPattern aPat1 `shouldBe` aPat1Expected
            desugarToPattern aPat2 `shouldBe` aPat2Expected
            desugarToPattern aPat3 `shouldBe` aPat3Expected
            desugarToPattern aPat4 `shouldBe` aPat4Expected
            desugarToPattern aPat5 `shouldBe` aPat5Expected
            desugarToPattern aPat6 `shouldBe` aPat6Expected
            desugarToPattern aPat7 `shouldBe` aPat7Expected
            desugarToPattern aPat8 `shouldBe` aPat8Expected
            desugarToPattern aPat9 `shouldBe` aPat9Expected
        it "should desugar LPat" $ do
            desugarToPattern lPat1 `shouldBe` lPat1Expected
            desugarToPattern lPat2 `shouldBe` lPat2Expected
            desugarToPattern lPat3 `shouldBe` lPat3Expected
        it "should desugar Pat" $ do
            desugarToPattern pat1 `shouldBe` pat1Expected
            desugarToPattern pat2 `shouldBe` pat2Expected
        it "should keep track of positions" $
            desugarToPattern (WithLocation aPat1 (sourceLocation 1 2 3 4)) `shouldBe`
            WithLocation (getValue aPat1Expected) (sourceLocation 1 2 3 4)
