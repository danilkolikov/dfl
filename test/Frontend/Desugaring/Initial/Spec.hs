{- |
Module      :  Frontend.Desugaring.Initial.Spec
Description :  Tests for desugaring of DFL grammar
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for the initial step of desugaring of DFL grammar
-}
module Frontend.Desugaring.Initial.Spec
    ( testSuite
    ) where

import qualified Frontend.Desugaring.Initial.ToConstTest as ToConst
import qualified Frontend.Desugaring.Initial.ToConstrTest as ToConstr
import qualified Frontend.Desugaring.Initial.ToConstraintTest as ToConstraint
import qualified Frontend.Desugaring.Initial.ToExpTest as ToExp
import qualified Frontend.Desugaring.Initial.ToIdentTest as ToIdent
import qualified Frontend.Desugaring.Initial.ToInstTest as ToInst
import qualified Frontend.Desugaring.Initial.ToNewConstrTest as ToNewConstr
import qualified Frontend.Desugaring.Initial.ToPatternTest as ToPattern
import qualified Frontend.Desugaring.Initial.ToSimpleClassTest as ToSimpleClass
import qualified Frontend.Desugaring.Initial.ToSimpleTypeTest as ToSimpleType
import qualified Frontend.Desugaring.Initial.ToTypeTest as ToType

testSuite :: IO ()
testSuite = do
    ToIdent.testSuite
    ToConst.testSuite
    ToType.testSuite
    ToConstraint.testSuite
    ToInst.testSuite
    ToSimpleClass.testSuite
    ToSimpleType.testSuite
    ToConstr.testSuite
    ToNewConstr.testSuite
    ToPattern.testSuite
    ToExp.testSuite
