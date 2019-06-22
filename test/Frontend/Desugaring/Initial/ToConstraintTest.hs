{- |
Module      :  Frontend.Desugaring.Initial.ToConstraintTest
Description :  Tests for desugaring of object to Constraint-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to Constraint-s
-}
module Frontend.Desugaring.Initial.ToConstraintTest
    ( testSuite
    , getConstraintExample
    ) where

import Test.Hspec

import qualified Data.List.NonEmpty as NE

import qualified Frontend.Desugaring.Initial.Ast as D (Constraint(..))
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToConstraint (desugarToConstraint)
import Frontend.Desugaring.Initial.ToIdentTest (getIdentExample)
import Frontend.Desugaring.Initial.ToTypeTest (getTypeExample)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Utils.RandomSelector

getConstraintExample ::
       RandomSelector (WithLocation Class, WithLocation D.Constraint)
getConstraintExample =
    selectFromRandom
        [ do (nameEx, nameRes) <- getIdentExample
             (argEx, argRes) <- getIdentExample
             withSameLocation $
                 return
                     (ClassSimple nameEx argEx, D.Constraint nameRes argRes [])
        , do (nameEx, nameRes) <- getIdentExample
             (argEx, argRes) <- getIdentExample
             (paramsEx, paramsRes) <- randomNonEmpty 2 getTypeExample
             withSameLocation $
                 return
                     ( ClassApplied nameEx argEx paramsEx
                     , D.Constraint nameRes argRes (NE.toList paramsRes))
        ]

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToConstraint" $
    it "should desugar constraints" $
    checkDesugaring desugarToConstraint getConstraintExample
