{- |
Module      :  Frontend.Desugaring.Initial.ToConstTest
Description :  Tests for desugaring of object to Const-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to Const-s
-}
module Frontend.Desugaring.Initial.ToConstTest
    ( testSuite
    , getConstExample
    ) where

import Test.Hspec

import Data.Functor (($>))

import Frontend.Desugaring.Initial.Ast (Const(..))
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToConst (DesugarToConst(..))
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Syntax.Token
import Frontend.Utils.RandomSelector

type ConstExample a = RandomSelector (WithLocation a, WithLocation Const)

class WithConstExamples a where
    getConstExample :: ConstExample a

instance WithConstExamples IntT where
    getConstExample = withSameLocation $ return (IntT 4, ConstInt 4)

instance WithConstExamples FloatT where
    getConstExample = withSameLocation $ return (FloatT 4.2, ConstFloat 4.2)

instance WithConstExamples CharT where
    getConstExample = withSameLocation $ return (CharT 'a', ConstChar 'a')

instance WithConstExamples StringT where
    getConstExample = withSameLocation $ return (StringT "ab", ConstString "ab")

instance WithConstExamples Literal where
    getConstExample =
        selectFromRandom
            [ do (ex, res) <- getConstExample
                 return (ex $> LiteralInteger ex, res)
            , do (ex, res) <- getConstExample
                 return (ex $> LiteralFloat ex, res)
            , do (ex, res) <- getConstExample
                 return (ex $> LiteralChar ex, res)
            , do (ex, res) <- getConstExample
                 return (ex $> LiteralString ex, res)
            ]

checkConstDesugaring ::
       (WithConstExamples a, DesugarToConst a) => ConstExample a -> Expectation
checkConstDesugaring = checkDesugaring desugarToConst

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToConst" $ do
        it "should desugar simple constants" $ do
            checkConstDesugaring (getConstExample :: ConstExample IntT)
            checkConstDesugaring (getConstExample :: ConstExample FloatT)
            checkConstDesugaring (getConstExample :: ConstExample CharT)
            checkConstDesugaring (getConstExample :: ConstExample StringT)
        it "should desugar literals" $
            checkConstDesugaring (getConstExample :: ConstExample Literal)
