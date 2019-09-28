{- |
Module      :  Spec
Description :  Tests for DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for DFL
-}
import qualified Frontend.Spec as Frontend
import qualified Util.Spec as Util

main :: IO ()
main = do
    Frontend.testSuite
    Util.testSuite
