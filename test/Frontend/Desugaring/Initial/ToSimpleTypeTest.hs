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

import Core.Ident
import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToIdentTest (getSimpleIdentExample)
import Frontend.Desugaring.Initial.ToSimpleType (desugarToSimpleType)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Utils.RandomSelector

getSimpleTypeExample ::
       RandomSelector (WithLocation SimpleType, WithLocation D.SimpleType)
getSimpleTypeExample = do
    (nameEx, nameRes) <- withSameLocation getSimpleIdentExample
    (paramsEx, paramsRes) <-
        randomList 5 $ withSameLocation getSimpleIdentExample
    withSameLocation $
        return
            ( SimpleType nameEx paramsEx
            , D.SimpleType
                  (IdentSimple <$> nameRes)
                  (map (IdentSimple <$>) paramsRes))

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToSimpleType" $
    it "should desugar SimpleType" $
    checkDesugaring desugarToSimpleType getSimpleTypeExample
