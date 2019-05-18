{- |
Module      :  Frontend.Syntax.Spec
Description :  Tests for the syntax analyser of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for the syntax analyser of DFL.
-}
module Frontend.Syntax.Spec where

import Frontend.Syntax.AnalyserTest as Analyser
import Frontend.Syntax.AstCheckerTest as AstChecker
import Frontend.Syntax.FixityResolutionTest as FixityResolution
import Frontend.Syntax.LayoutTest as Layout
import Frontend.Syntax.LexerTest as Lexer
import Frontend.Syntax.ParserTest as Parser

testSuite :: IO ()
testSuite = do
    Lexer.testSuite
    Layout.testSuite
    Parser.testSuite
    FixityResolution.testSuite
    AstChecker.testSuite
    Analyser.testSuite
