{- |
Module      :  Frontend.Syntax.Spec
Description :  Tests for the syntax analyser of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for the syntax analyser of DFL.
-}
module Frontend.Syntax.Spec where

import qualified Frontend.Syntax.AstCheckerTest as AstChecker
import qualified Frontend.Syntax.FixityResolutionTest as FixityResolution
import qualified Frontend.Syntax.LayoutTest as Layout
import qualified Frontend.Syntax.LexerTest as Lexer
import qualified Frontend.Syntax.ParserTest as Parser
import qualified Frontend.Syntax.ProcessorTest as Processor

testSuite :: IO ()
testSuite = do
    Lexer.testSuite
    Layout.testSuite
    Parser.testSuite
    FixityResolution.testSuite
    AstChecker.testSuite
    Processor.testSuite
