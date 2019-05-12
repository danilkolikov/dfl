{- |
Module      :  Spec
Description :  Tests for DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for DFL
-}
import Frontend.Spec as Frontend

main :: IO ()
main = do
    Frontend.testSuite
