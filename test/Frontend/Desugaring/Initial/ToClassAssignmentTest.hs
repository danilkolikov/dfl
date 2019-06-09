{- |
Module      :  Frontend.Desugaring.Initial.ToClassAssignmentTest
Description :  Tests for desugaring of object to ClassAssignment-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to ClassAssignment-s
-}
module Frontend.Desugaring.Initial.ToClassAssignmentTest
    ( testSuite
    , getClassAssignmentExample
    ) where

import Test.Hspec

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE

import qualified Frontend.Desugaring.Initial.Ast as D (ClassAssignment(..))
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToClassAssignment (desugarToClassAssignment)
import Frontend.Desugaring.Initial.ToExpTest
    ( getExpExample
    , getFunLHSExample
    , getGenDeclExample
    )
import Frontend.Desugaring.Initial.ToIdentTest (getIdentExample)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Utils.RandomSelector

getClassAssignmentExample ::
       RandomSelector (WithLocation CDecl, [WithLocation D.ClassAssignment])
getClassAssignmentExample =
    selectFromRandom
        [ do (genDeclEx, genDeclRes) <- getGenDeclExample D.ClassAssignmentType
             return (genDeclEx $> CDeclGenDecl genDeclEx, genDeclRes)
        , do (lhsEx, (nameRes, patsRes)) <- getFunLHSExample
             (rhsEx, rhsRes) <- getExpExample
             loc <- getRandomSourceLocation
             let wrap = (`WithLocation` loc)
             return
                 ( wrap $ CDeclFunction (wrap $ Left lhsEx) rhsEx
                 , [ wrap $
                     D.ClassAssignmentName nameRes (NE.toList patsRes) rhsRes
                   ])
        , do (nameEx, nameRes) <- getIdentExample
             (rhsEx, rhsRes) <- getExpExample
             loc <- getRandomSourceLocation
             let wrap = (`WithLocation` loc)
             return
                 ( wrap $ CDeclFunction (Right <$> nameEx) rhsEx
                 , [wrap $ D.ClassAssignmentName nameRes [] rhsRes])
        ]

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToClassAssignment" $
    it "should desugar CDecl" $
    checkDesugaring 10 2 desugarToClassAssignment getClassAssignmentExample
