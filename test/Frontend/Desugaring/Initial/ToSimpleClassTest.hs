{- |
Module      :  Frontend.Desugaring.Initial.ToSimpleClassTest
Description :  Tests for desugaring of object to SimpleClass-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to SimpleClass-s
-}
module Frontend.Desugaring.Initial.ToSimpleClassTest
    ( testSuite
    , getSimpleClassExample
    ) where

import Test.Hspec

import Core.Ident
import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToIdentTest
    ( getIdentExample
    , getSimpleIdentExample
    )
import Frontend.Desugaring.Initial.ToSimpleClass (desugarToSimpleClass)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Utils.RandomSelector

getSimpleClassExample ::
       RandomSelector (WithLocation SimpleClass, WithLocation D.SimpleClass)
getSimpleClassExample = do
    (nameEx, nameRes) <- getIdentExample
    (paramEx, paramRes) <- withSameLocation getSimpleIdentExample
    withSameLocation $
        return
            ( SimpleClass nameEx paramEx
            , D.SimpleClass nameRes (IdentSimple <$> paramRes))

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToSimpleClass" $
    it "should desugar SimpleClass" $
    checkDesugaring desugarToSimpleClass getSimpleClassExample
