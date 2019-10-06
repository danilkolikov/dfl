{- |
Module      :  Frontend.Desugaring.Initial.ToNewConstrTest
Description :  Tests for desugaring of object to NewConstr-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to NewConstr-s
-}
module Frontend.Desugaring.Initial.ToNewConstrTest
    ( testSuite
    , getNewConstrExample
    ) where

import Test.Hspec

import Core.Ident
import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToIdentTest (getSimpleIdentExample)
import Frontend.Desugaring.Initial.ToNewConstr (desugarToNewConstr)
import Frontend.Desugaring.Initial.ToTypeTest (getTypeExample)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Utils.RandomSelector

getNewConstrExample ::
       RandomSelector (WithLocation NewConstr, WithLocation D.NewConstr)
getNewConstrExample =
    selectFromRandom
        [ do (nameEx, nameRes) <- withSameLocation getSimpleIdentExample
             (typeEx, typeRes) <- getTypeExample
             withSameLocation $
                 return
                     ( NewConstrSimple nameEx typeEx
                     , D.NewConstrSimple (IdentSimple <$> nameRes) typeRes)
        , do (nameEx, nameRes) <- withSameLocation getSimpleIdentExample
             (fieldEx, fieldRes) <- withSameLocation getSimpleIdentExample
             (typeEx, typeRes) <- getTypeExample
             withSameLocation $
                 return
                     ( NewConstrNamed nameEx fieldEx typeEx
                     , D.NewConstrRecord
                           (IdentSimple <$> nameRes)
                           (IdentSimple <$> fieldRes)
                           typeRes)
        ]

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToNewConstr" $
    it "should desugar NewConstr" $
    checkDesugaring desugarToNewConstr getNewConstrExample
