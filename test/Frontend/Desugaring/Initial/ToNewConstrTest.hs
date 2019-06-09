{- |
Module      :  Frontend.Desugaring.Initial.ToNewConstrTest
Description :  Tests for desugaring of object to NewConstr-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to NewConstr-s
-}
module Frontend.Desugaring.Initial.ToNewConstrTest
    ( testSuite
    , getNewConstrExamples
    ) where

import Test.Hspec

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToIdentTest (getIdentExample)
import Frontend.Desugaring.Initial.ToNewConstr (desugarToNewConstr)
import Frontend.Desugaring.Initial.ToTypeTest (getTypeExample)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Utils.RandomSelector

getNewConstrExamples ::
       RandomSelector (WithLocation NewConstr, WithLocation D.NewConstr)
getNewConstrExamples =
    selectFromRandom
        [ do (nameEx, nameRes) <- getIdentExample
             (typeEx, typeRes) <- getTypeExample
             withSameLocation $
                 return
                     ( NewConstrSimple nameEx typeEx
                     , D.NewConstrSimple nameRes typeRes)
        , do (nameEx, nameRes) <- getIdentExample
             (fieldEx, fieldRes) <- getIdentExample
             (typeEx, typeRes) <- getTypeExample
             withSameLocation $
                 return
                     ( NewConstrNamed nameEx fieldEx typeEx
                     , D.NewConstrRecord nameRes fieldRes typeRes)
        ]

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToNewConstr" $
    it "should desugar NewConstr" $
    checkDesugaring 10 3 desugarToNewConstr getNewConstrExamples
