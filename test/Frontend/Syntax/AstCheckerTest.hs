{- |
Module      :  Frontend.Syntax.AstCheckerTest
Description :  Tests for AstChecker
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for AstChecker.
-}
module Frontend.Syntax.AstCheckerTest where

import Test.Hspec

import Frontend.Syntax.Ast
import Frontend.Syntax.AstChecker
import Frontend.Syntax.Position (WithLocation(..), sourceLocation)
import Frontend.Syntax.Token (VarId(..))

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
        it "doesn't rise error when all type variables are different" $ do
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
