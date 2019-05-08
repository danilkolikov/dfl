module Frontend.Grammar.LexerTest where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

import qualified Data.HashMap.Lazy as HM
import Frontend.Grammar.Lexer (Lexable(..), Lexer, programLexer, whitespace)
import Frontend.Grammar.Position (WL, WithLocation(..), sourceLocation)
import Frontend.Grammar.Token

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
                    "Frontend.Grammar"
                    (TokenName [ConId "Frontend"] $ NameConId (ConId "Grammar"))
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
    shouldParseProgram :: String -> [WL Token] -> Expectation
    shouldParseProgram = shouldParse' programLexer
