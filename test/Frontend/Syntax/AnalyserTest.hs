{- |
Module      :  Frontend.Syntax.AnalyserTest
Description :  Tests for the Syntax Analyser of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for the syntax analyser of DFL
-}
module Frontend.Syntax.AnalyserTest where

import Test.Hspec

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE
import Frontend.Syntax.Analyser
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..), sourceLocation)
import Frontend.Syntax.Token

testSuite :: IO ()
testSuite =
    hspec $ do
        describe "syntaxAnalyser" $ do
            it "fails on an empty program" $
                runSyntaxAnalyser "" (AnalyserState "" HM.empty) `shouldBe`
                Left
                    (SyntaxErrorParser
                         (sourceLocation 1 1 1 1)
                         (Just (TokenEOF EOF))
                         ["KeywordModule", "SpecialLCurly"])
            it "parses programs" $ do
                let state = AnalyserState "" HM.empty
                runSyntaxAnalyser "module Foo.Bar where" state `shouldBe`
                    Right
                        ( ModuleExplicit
                              (WithLocation
                                   (Qualified [ConId "Foo"] (ConId "Bar"))
                                   (sourceLocation 1 8 1 15))
                              Nothing
                              (WithLocation
                                   (Body [] [])
                                   (sourceLocation 1 21 1 21))
                        , state)
                runSyntaxAnalyser "x = 1" state `shouldBe`
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
                      in Right (res, state))
