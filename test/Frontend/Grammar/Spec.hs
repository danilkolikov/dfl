module Frontend.Grammar.Spec where

import Frontend.Grammar.LexerTest as Lexer
import Frontend.Grammar.LayoutTest as Layout

testSuite :: IO ()
testSuite = do
    Lexer.testSuite
    Layout.testSuite
