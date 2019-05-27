{- |
Module      :  Frontend.Desugaring.Spec
Description :  Tests for desugaring of DFL grammar
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of DFL grammar
-}
module Frontend.Desugaring.Spec
    ( testSuite
    ) where

import qualified Frontend.Desugaring.ToConstTest as ToConst
import qualified Frontend.Desugaring.ToConstrTest as ToConstr
import qualified Frontend.Desugaring.ToConstraintTest as ToConstraint
import qualified Frontend.Desugaring.ToIdentTest as ToIdent
import qualified Frontend.Desugaring.ToInstTest as ToInst
import qualified Frontend.Desugaring.ToNewConstrTest as ToNewConstr
import qualified Frontend.Desugaring.ToPatternTest as ToPattern
import qualified Frontend.Desugaring.ToSimpleClassTest as ToSimpleClass
import qualified Frontend.Desugaring.ToSimpleTypeTest as ToSimpleType
import qualified Frontend.Desugaring.ToTypeTest as ToType

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
