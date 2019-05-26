{- |
Module      :  Frontend.Syntax.AstCheckerTest
Description :  Tests for AstChecker
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for AstChecker.
-}
module Frontend.Syntax.AstCheckerTest where

import Test.Hspec

import qualified Data.List.NonEmpty as NE (NonEmpty(..))

import Frontend.Syntax.Ast
import Frontend.Syntax.AstChecker
import Frontend.Syntax.Position
    ( WithLocation(..)
    , sourceLocation
    , withDummyLocation
    )
import Frontend.Syntax.Token (IntT(..), VarId(..))

testSuite :: IO ()
testSuite =
    hspec $
    describe "checkAst" $ do
        let tyVar name pos =
                WithLocation (VarId name) (sourceLocation 1 pos 1 (pos + 1))
        it "raises error when instance declaration has 2 same type variables" $ do
            checkAst
                (InstNamed
                     (WithLocation GTyConUnit (sourceLocation 1 1 1 3))
                     [tyVar "x" 4, tyVar "y" 5, tyVar "x" 6]) `shouldBe`
                Left (AstCheckerErrorSameTyVar (tyVar "x" 4) (tyVar "x" 6))
            checkAst (InstTuple (tyVar "x" 4) (tyVar "y" 5) [tyVar "x" 6]) `shouldBe`
                Left (AstCheckerErrorSameTyVar (tyVar "x" 4) (tyVar "x" 6))
            checkAst (InstFunction (tyVar "x" 4) (tyVar "x" 6)) `shouldBe`
                Left (AstCheckerErrorSameTyVar (tyVar "x" 4) (tyVar "x" 6))
        it "doesn't raise error when all type variables are different" $ do
            checkAst
                (InstNamed
                     (WithLocation GTyConUnit (sourceLocation 1 1 1 3))
                     [tyVar "x" 4, tyVar "y" 5, tyVar "z" 6]) `shouldBe`
                Right ()
            checkAst (InstTuple (tyVar "x" 4) (tyVar "y" 5) [tyVar "z" 6]) `shouldBe`
                Right ()
            checkAst (InstFunction (tyVar "x" 4) (tyVar "y" 6)) `shouldBe`
                Right ()
            checkAst (InstList (tyVar "x" 4)) `shouldBe` Right ()
        let aExp =
                withDummyLocation .
                AExpLiteral .
                withDummyLocation . LiteralInteger . withDummyLocation . IntT $
                42
            exp' =
                withDummyLocation .
                ExpSimple . withDummyLocation . InfixExpLExp . withDummyLocation $
                LExpApplication (aExp NE.:| [])
        it "raises error when the last statement in do is not an expression" $ do
            let pat =
                    withDummyLocation .
                    PatSimple .
                    withDummyLocation .
                    LPatNegated . withDummyLocation . Left . IntT $
                    42
                stmt1 = withDummyLocation $ StmtPat pat exp'
            checkAst (LExpDo (stmt1 NE.:| [])) `shouldBe`
                Left (AstCheckerErrorLastStatement stmt1)
            let stmt2 = withDummyLocation $ StmtLet []
            checkAst (LExpDo (stmt2 NE.:| [])) `shouldBe`
                Left (AstCheckerErrorLastStatement stmt2)
        it "doesn't raise error when the last statement in do is an expression" $ do
            let stmt = withDummyLocation $ StmtExp exp'
            checkAst (LExpDo (stmt NE.:| [])) `shouldBe` Right ()
