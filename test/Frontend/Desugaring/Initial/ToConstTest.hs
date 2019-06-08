{- |
Module      :  Frontend.Desugaring.Initial.ToConstTest
Description :  Tests for desugaring of object to Const-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to Const-s
-}
module Frontend.Desugaring.Initial.ToConstTest
    ( testSuite
    ) where

import Test.Hspec

import Frontend.Desugaring.Initial.Ast (Const(..))
import Frontend.Desugaring.Initial.ToConst (desugarToConst)
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
    describe "desugarToConst" $ do
        it "should desugar simple constants" $ do
            desugarToConst (withDummyLocation $ IntT 4) `shouldBe`
                withDummyLocation (ConstInt 4)
            desugarToConst (withDummyLocation $ FloatT 4.2) `shouldBe`
                withDummyLocation (ConstFloat 4.2)
            desugarToConst (withDummyLocation $ CharT 'a') `shouldBe`
                withDummyLocation (ConstChar 'a')
            desugarToConst (withDummyLocation $ StringT "ab") `shouldBe`
                withDummyLocation (ConstString "ab")
        it "should desugar literals" $ do
            desugarToConst
                (withDummyLocation $ LiteralInteger (withDummyLocation (IntT 4))) `shouldBe`
                withDummyLocation (ConstInt 4)
            desugarToConst
                (withDummyLocation $
                 LiteralFloat (withDummyLocation (FloatT 4.2))) `shouldBe`
                withDummyLocation (ConstFloat 4.2)
            desugarToConst
                (withDummyLocation $ LiteralChar (withDummyLocation (CharT 'a'))) `shouldBe`
                withDummyLocation (ConstChar 'a')
            desugarToConst
                (withDummyLocation $
                 LiteralString (withDummyLocation (StringT "ab"))) `shouldBe`
                withDummyLocation (ConstString "ab")
        it "should keep track of locations" $
            desugarToConst (WithLocation (IntT 4) (sourceLocation 1 2 3 4)) `shouldBe`
            WithLocation (ConstInt 4) (sourceLocation 1 2 3 4)
