{- |
Module      :  Frontend.Syntax.ProcessorTest
Description :  Tests for the syntax processor of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for the syntax processor of DFL
-}
module Frontend.Syntax.ProcessorTest where

import Test.Hspec

import Data.Bifunctor (bimap, first)
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..), sourceLocation)
import Frontend.Syntax.Processor
import Frontend.Syntax.Token

data SyntaxError
    = SyntaxErrorLexer LexicalError
    | SyntaxErrorParser ParserError
    | SyntaxErrorFixity FixityResolutionError
    deriving (Eq, Show)

testSuite :: IO ()
testSuite =
    hspec $ do
        let runSyntaxAnalyser content = do
                tokens <- first SyntaxErrorLexer $ lexicalAnalysis "" content
                ast <- first SyntaxErrorParser $ parsing "" tokens
                bimap SyntaxErrorFixity getFixityResolutionOutputAst $
                    fixityResolution HM.empty ast
        describe "syntaxAnalyser" $ do
            it "fails on an empty program" $
                runSyntaxAnalyser "" `shouldBe`
                Left
                    (SyntaxErrorParser $
                     ParserErrorParser
                         (sourceLocation 1 1 1 1)
                         (Just (TokenEOF EOF))
                         ["KeywordModule", "SpecialLCurly"])
            it "parses programs" $ do
                runSyntaxAnalyser "module Foo.Bar where" `shouldBe`
                    Right
                        (ModuleExplicit
                             (WithLocation
                                  (Qualified [ConId "Foo"] (ConId "Bar"))
                                  (sourceLocation 1 8 1 15))
                             Nothing
                             (WithLocation
                                  (Body [] [])
                                  (sourceLocation 1 21 1 21)))
                runSyntaxAnalyser "x = 1" `shouldBe`
                    (let xVar =
                             WithLocation
                                 (FuncLabelId $ VarId "x")
                                 (sourceLocation 1 1 1 2)
                         xAPat =
                             WithLocation
                                 (APatVariable xVar Nothing)
                                 (sourceLocation 1 1 1 2)
                         xLPat =
                             WithLocation
                                 (LPatSimple xAPat)
                                 (sourceLocation 1 1 1 2)
                         lhs =
                             WithLocation
                                 (Right (PatSimple xLPat))
                                 (sourceLocation 1 1 1 2)
                         oneLiteral =
                             WithLocation
                                 (LiteralInteger
                                      (WithLocation
                                           (IntT 1)
                                           (sourceLocation 1 5 1 6)))
                                 (sourceLocation 1 5 1 6)
                         oneLExp =
                             WithLocation
                                 (LExpApplication
                                      ((WithLocation
                                            (AExpLiteral oneLiteral)
                                            (sourceLocation 1 5 1 6)) NE.:|
                                       []))
                                 (sourceLocation 1 5 1 6)
                         one =
                             WithLocation
                                 (ExpSimple
                                      (WithLocation
                                           (InfixExpLExp oneLExp)
                                           (sourceLocation 1 5 1 6)))
                                 (sourceLocation 1 5 1 6)
                         rhs =
                             WithLocation
                                 (RHSSimple one [])
                                 (sourceLocation 1 3 1 6)
                         expr =
                             WithLocation
                                 (DeclFunction lhs rhs)
                                 (sourceLocation 1 1 1 6)
                         res =
                             ModuleImplicit
                                 (WithLocation
                                      (Body
                                           []
                                           [ WithLocation
                                                 (TopDeclDecl expr)
                                                 (sourceLocation 1 1 1 6)
                                           ])
                                      (sourceLocation 1 1 1 6))
                      in Right res)
