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
import qualified Frontend.Desugaring.ToIdentTest as ToIdent

testSuite :: IO ()
testSuite = do
    ToIdent.testSuite
    ToConst.testSuite
