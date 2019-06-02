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

import qualified Frontend.Desugaring.Final.ProcessorTest as Processor
import qualified Frontend.Desugaring.Final.NewTypeDesugaringTest as NewType
import qualified Frontend.Desugaring.Final.TypeSynonymDesugaringTest as TypeSynonym

testSuite :: IO ()
testSuite = do
    Processor.testSuite
    TypeSynonym.testSuite
    NewType.testSuite
