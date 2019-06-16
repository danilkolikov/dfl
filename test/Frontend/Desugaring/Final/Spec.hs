{- |
Module      :  Frontend.Desugaring.Final.Spec
Description :  Tests for desugaring of DFL grammar
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for the final step of desugaring of DFL grammar
-}
module Frontend.Desugaring.Final.Spec
    ( testSuite
    ) where

import qualified Frontend.Desugaring.Final.DataTypeDesugaringTest as DataType
import qualified Frontend.Desugaring.Final.ExpressionDesugaringAssignmentTest as ExpressionDesugaringAssignment
import qualified Frontend.Desugaring.Final.ExpressionDesugaringBaseTest as ExpressionDesugaringBase
import qualified Frontend.Desugaring.Final.ExpressionDesugaringCaseTest as ExpressionDesugaringCase
import qualified Frontend.Desugaring.Final.ExpressionDesugaringStmtTest as ExpressionDesugaringStmt
import qualified Frontend.Desugaring.Final.ProcessorTest as Processor
import qualified Frontend.Desugaring.Final.RecordDesugaringTest as RecordDesugaring
import qualified Frontend.Desugaring.Final.TypeSynonymDesugaringTest as TypeSynonym

testSuite :: IO ()
testSuite = do
    Processor.testSuite
    TypeSynonym.testSuite
    DataType.testSuite
    RecordDesugaring.testSuite
    ExpressionDesugaringBase.testSuite
    ExpressionDesugaringStmt.testSuite
    ExpressionDesugaringAssignment.testSuite
    ExpressionDesugaringCase.testSuite
