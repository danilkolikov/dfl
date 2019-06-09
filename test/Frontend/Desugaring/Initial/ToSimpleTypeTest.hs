{- |
Module      :  Frontend.Desugaring.Initial.ToSimpleTypeTest
Description :  Tests for desugaring of object to SimpleType-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to SimpleType-s
-}
module Frontend.Desugaring.Initial.ToSimpleTypeTest
    ( testSuite
    , getSimpleTypeExample
    ) where

import Test.Hspec

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToIdentTest (getIdentExample)
import Frontend.Desugaring.Initial.ToSimpleType (desugarToSimpleType)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Utils.RandomSelector

getSimpleTypeExample ::
       RandomSelector (WithLocation SimpleType, WithLocation D.SimpleType)
getSimpleTypeExample = do
    (nameEx, nameRes) <- getIdentExample
    (paramsEx, paramsRes) <- randomList 5 getIdentExample
    withSameLocation $
        return (SimpleType nameEx paramsEx, D.SimpleType nameRes paramsRes)

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToSimpleType" $
    it "should desugar SimpleType" $
    checkDesugaring 5 1 desugarToSimpleType getSimpleTypeExample
