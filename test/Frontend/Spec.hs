module Frontend.Spec where

import Frontend.Grammar.Spec as G

testSuite :: IO ()
testSuite = do
    G.testSuite
