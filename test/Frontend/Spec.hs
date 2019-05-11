module Frontend.Spec where

import Frontend.Grammar.Spec as Grammar

testSuite :: IO ()
testSuite = do
    Grammar.testSuite
