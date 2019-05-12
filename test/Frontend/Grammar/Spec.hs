module Frontend.Grammar.Spec where

import Frontend.Grammar.LayoutTest as Layout
import Frontend.Grammar.LexerTest as Lexer
import Frontend.Grammar.ParserTest as Parser

testSuite :: IO ()
testSuite = do
    Lexer.testSuite
    Layout.testSuite
    Parser.testSuite
