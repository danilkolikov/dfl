{- |
Module      :  Frontend.Desugaring.Initial.ToInstAssignmentTest
Description :  Tests for desugaring of object to InstAssignment-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to InstAssignment-s
-}
module Frontend.Desugaring.Initial.ToInstAssignmentTest
    ( testSuite
    , getInstAssignmentExample
    ) where

import Test.Hspec

import qualified Data.List.NonEmpty as NE

import qualified Frontend.Desugaring.Initial.Ast as D (InstAssignment(..))
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToExpTest (getExpExample, getFunLHSExample)
import Frontend.Desugaring.Initial.ToIdentTest (getIdentExample)
import Frontend.Desugaring.Initial.ToInstAssignment (desugarToInstAssignment)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Utils.RandomSelector

getInstAssignmentExample ::
       RandomSelector (WithLocation IDecl, WithLocation D.InstAssignment)
getInstAssignmentExample =
    selectFromRandom
        [ do (lhsEx, (nameRes, patsRes)) <- getFunLHSExample
             (rhsEx, rhsRes) <- getExpExample
             loc <- getRandomSourceLocation
             let wrap = (`WithLocation` loc)
             return
                 ( wrap $ IDeclFunction (wrap $ Left lhsEx) rhsEx
                 , wrap $
                   D.InstAssignmentName nameRes (NE.toList patsRes) rhsRes)
        , do (nameEx, nameRes) <- getIdentExample
             (rhsEx, rhsRes) <- getExpExample
             loc <- getRandomSourceLocation
             let wrap = (`WithLocation` loc)
             return
                 ( wrap $ IDeclFunction (Right <$> nameEx) rhsEx
                 , wrap $ D.InstAssignmentName nameRes [] rhsRes)
        ]

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToInstAssignment" $
    it "should desugar IDecl" $
    checkDesugaring 10 2 desugarToInstAssignment getInstAssignmentExample
