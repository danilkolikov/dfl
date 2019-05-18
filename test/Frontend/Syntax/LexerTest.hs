{- |
Module      :  Frontend.Syntax.LexerTest
Description :  Tests for lexer
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for the lexer of DFL.
-}
module Frontend.Syntax.LexerTest where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

import qualified Data.HashMap.Lazy as HM
import Frontend.Syntax.Lexer
    ( Lexable(..)
    , Lexer
    , programLexer
    , sourceLexer
    , whitespace
    )
import Frontend.Syntax.Position
    ( WithLocation(..)
    , dummyLocation
    , sourceLocation
    )
import Frontend.Syntax.Token

testSuite :: IO ()
testSuite =
    hspec $ do
        describe "lexeme" $ do
            it "parses operators" $ shouldParseReserved TokenOperator operators
            it "parses keywords" $ shouldParseReserved TokenKeyword keywords
            it "parses special symbols" $
                shouldParseReserved
                    TokenSpecial
                    (HM.fromList . map (\(c, r) -> ([c], r)) . HM.toList $
                     specialSymbols)
            it "parses floats" $ do
                shouldParseLexeme "123.456" (TokenFloat (FloatT 123.456))
                shouldParseLexeme "1e5" (TokenFloat (FloatT 1e5))
                shouldParseLexeme "1e+5" (TokenFloat (FloatT 1e+5))
                shouldParseLexeme "1e-5" (TokenFloat (FloatT 1e-5))
                shouldParseLexeme "1.1e2" (TokenFloat (FloatT 1.1e2))
                shouldParseLexeme "1.1e+2" (TokenFloat (FloatT 1.1e+2))
                shouldParseLexeme "1.1e-2" (TokenFloat (FloatT 1.1e-2))
                shouldParseLexeme "1E5" (TokenFloat (FloatT 1E5))
                shouldParseLexeme "1E+5" (TokenFloat (FloatT 1E+5))
                shouldParseLexeme "1E-5" (TokenFloat (FloatT 1E-5))
                shouldParseLexeme "1.1E2" (TokenFloat (FloatT 1.1E2))
                shouldParseLexeme "1.1E+2" (TokenFloat (FloatT 1.1E2))
                shouldParseLexeme "1.1E-2" (TokenFloat (FloatT 1.1E-2))
            it "parses integers" $ do
                shouldParseLexeme "12345" (TokenInteger (IntT 12345))
                shouldParseLexeme "0o123" (TokenInteger (IntT 0o123))
                shouldParseLexeme "0O234" (TokenInteger (IntT 0O234))
                shouldParseLexeme "0xdeadbee" (TokenInteger (IntT 0xdeadbee))
                shouldParseLexeme "0XDEADBEE" (TokenInteger (IntT 0XDEADBEE))
            it "parses chars" $ do
                shouldParseLexeme "'a'" (TokenChar (CharT 'a'))
                shouldParseLexeme "' ''" (TokenChar (CharT ' '))
                shouldParseLexeme "'\\n'" (TokenChar (CharT '\n'))
                shouldParseLexeme "'\\\\'" (TokenChar (CharT '\\'))
            it "parses strings" $ do
                shouldParseLexeme
                    "\"a \\n \\\\ \\\"\""
                    (TokenString (StringT "a \n \\ \""))
                shouldParseLexeme
                    "\"abc\\     \n    \\de\\   \\f\""
                    (TokenString (StringT "abcdef"))
            it "parses identifiers" $ do
                shouldParseLexeme
                    "ofCourse"
                    (TokenName [] $ NameVarId (VarId "ofCourse"))
                shouldParseLexeme
                    "Prelude.mod"
                    (TokenName [ConId "Prelude"] $ NameVarId (VarId "mod"))
                shouldParseLexeme
                    "Bool"
                    (TokenName [] $ NameConId (ConId "Bool"))
                shouldParseLexeme
                    "Frontend.Syntax"
                    (TokenName [ConId "Frontend"] $ NameConId (ConId "Syntax"))
                shouldParseLexeme "-" (TokenName [] $ NameVarSym (VarSym "-"))
                shouldParseLexeme
                    "Prelude.+"
                    (TokenName [ConId "Prelude"] $ NameVarSym (VarSym "+"))
                shouldParseLexeme ":|" (TokenName [] $ NameConSym (ConSym ":|"))
                shouldParseLexeme
                    "Prelude.:|"
                    (TokenName [ConId "Prelude"] $ NameConSym (ConSym ":|"))
            it "parses EOF" $ shouldParseLexeme "" (TokenEOF EOF)
        describe "whitespace" $ do
            it "parses missing whitespace" $ shouldParseWhitespace ""
            it "parses white chars" $ shouldParseWhitespace "      \t \n    "
            it "parses single line comments" $ shouldParseWhitespace "-- abcd"
            it "parses arbitrary single line comments" $
                shouldParseWhitespace "-- -- -- -- abcd"
            it "parses multiline line comments" $
                shouldParseWhitespace "{- abcd -}"
            it "parses nested multiline line comments" $
                shouldParseWhitespace "{- abcd {- def -}-}"
        describe "parseProgram" $
            it "tracks token positions" $ do
                shouldParseProgram "" []
                shouldParseProgram
                    " { } "
                    [ WithLocation
                          (TokenSpecial SpecialLCurly)
                          (sourceLocation 1 2 1 3)
                    , WithLocation
                          (TokenSpecial SpecialRCurly)
                          (sourceLocation 1 4 1 5)
                    ]
                shouldParseProgram
                    " module Math where\n  pi = 3  Prelude.+   0.14 \n\nT.e    =     2   "
                    [ WithLocation
                          (TokenKeyword KeywordModule)
                          (sourceLocation 1 2 1 8)
                    , WithLocation
                          (TokenName [] (NameConId (ConId "Math")))
                          (sourceLocation 1 9 1 13)
                    , WithLocation
                          (TokenKeyword KeywordWhere)
                          (sourceLocation 1 14 1 19)
                    , WithLocation
                          (TokenName [] (NameVarId (VarId "pi")))
                          (sourceLocation 2 3 2 5)
                    , WithLocation
                          (TokenOperator OperatorEq)
                          (sourceLocation 2 6 2 7)
                    , WithLocation
                          (TokenInteger (IntT 3))
                          (sourceLocation 2 8 2 9)
                    , WithLocation
                          (TokenName [ConId "Prelude"] (NameVarSym (VarSym "+")))
                          (sourceLocation 2 11 2 20)
                    , WithLocation
                          (TokenFloat (FloatT 0.14))
                          (sourceLocation 2 23 2 27)
                    , WithLocation
                          (TokenName [ConId "T"] (NameVarId (VarId "e")))
                          (sourceLocation 4 1 4 4)
                    , WithLocation
                          (TokenOperator OperatorEq)
                          (sourceLocation 4 8 4 9)
                    , WithLocation
                          (TokenInteger (IntT 2))
                          (sourceLocation 4 14 4 15)
                    ]
        describe "sourceLexer" $ do
            let lCurly = WithLocation (TokenSpecial SpecialLCurly) dummyLocation
                rCurly = WithLocation (TokenSpecial SpecialRCurly) dummyLocation
                semicolon =
                    WithLocation (TokenSpecial SpecialSemicolon) dummyLocation
            it "parses empty program" $
                sourceLexerShouldBe
                    ""
                    [WithLocation (TokenEOF EOF) (sourceLocation 1 1 1 1)]
            it "inserts brackets" $ do
                sourceLexerShouldBe
                    "x = 5\ny = 6.5"
                    [ lCurly
                    , WithLocation
                          (TokenName [] (NameVarId (VarId "x")))
                          (sourceLocation 1 1 1 2)
                    , WithLocation
                          (TokenOperator OperatorEq)
                          (sourceLocation 1 3 1 4)
                    , WithLocation
                          (TokenInteger (IntT 5))
                          (sourceLocation 1 5 1 6)
                    , semicolon
                    , WithLocation
                          (TokenName [] (NameVarId (VarId "y")))
                          (sourceLocation 2 1 2 2)
                    , WithLocation
                          (TokenOperator OperatorEq)
                          (sourceLocation 2 3 2 4)
                    , WithLocation
                          (TokenFloat (FloatT 6.5))
                          (sourceLocation 2 5 2 8)
                    , rCurly
                    , WithLocation (TokenEOF EOF) (sourceLocation 2 8 2 8)
                    ]
                sourceLexerShouldBe
                    "do \n  x\n  y"
                    [ lCurly
                    , WithLocation
                          (TokenKeyword KeywordDo)
                          (sourceLocation 1 1 1 3)
                    , lCurly
                    , WithLocation
                          (TokenName [] (NameVarId (VarId "x")))
                          (sourceLocation 2 3 2 4)
                    , semicolon
                    , WithLocation
                          (TokenName [] (NameVarId (VarId "y")))
                          (sourceLocation 3 3 3 4)
                    , rCurly
                    , rCurly
                    , WithLocation (TokenEOF EOF) (sourceLocation 3 4 3 4)
                    ]
            it "inserts brackets after keywords" $
                sourceLexerShouldBe
                    "let x = 5 in x"
                    [ WithLocation (TokenSpecial SpecialLCurly) dummyLocation
                    , WithLocation
                          (TokenKeyword KeywordLet)
                          (sourceLocation 1 1 1 4)
                    , WithLocation (TokenSpecial SpecialLCurly) dummyLocation
                    , WithLocation
                          (TokenName [] (NameVarId (VarId "x")))
                          (sourceLocation 1 5 1 6)
                    , WithLocation
                          (TokenOperator OperatorEq)
                          (sourceLocation 1 7 1 8)
                    , WithLocation
                          (TokenInteger (IntT 5))
                          (sourceLocation 1 9 1 10)
                    , WithLocation (TokenSpecial SpecialRCurly) dummyLocation
                    , WithLocation
                          (TokenKeyword KeywordIn)
                          (sourceLocation 1 11 1 13)
                    , WithLocation
                          (TokenName [] (NameVarId (VarId "x")))
                          (sourceLocation 1 14 1 15)
                    , WithLocation (TokenSpecial SpecialRCurly) dummyLocation
                    , WithLocation (TokenEOF EOF) (sourceLocation 1 15 1 15)
                    ]
            it "fails on wrong bracket sequence" $ do
                sourceLexerShouldFailOn "{"
                sourceLexerShouldFailOn "}"
                sourceLexerShouldFailOn "let x = 5} in x"
  where
    shouldParse' :: (Show a, Eq a) => Lexer a -> String -> a -> Expectation
    shouldParse' lexer' s = shouldParse $ parse lexer' "" s
    shouldParseLexeme :: String -> Token -> Expectation
    shouldParseLexeme = shouldParse' lexer
    shouldParseWhitespace :: String -> Expectation
    shouldParseWhitespace s = shouldParse' whitespace s ()
    shouldParseReserved :: (a -> Token) -> HM.HashMap String a -> Expectation
    shouldParseReserved wrapper =
        mapM_ (\(s, x) -> shouldParseLexeme s (wrapper x)) . HM.toList
    shouldParseProgram :: String -> [WithLocation Token] -> Expectation
    shouldParseProgram = shouldParse' programLexer
    sourceLexerShouldBe :: String -> [WithLocation Token] -> Expectation
    sourceLexerShouldBe = shouldParse' sourceLexer
    sourceLexerShouldFailOn :: String -> Expectation
    sourceLexerShouldFailOn = shouldFailOn $ parse sourceLexer ""
