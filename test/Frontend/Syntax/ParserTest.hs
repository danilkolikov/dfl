{- |
Module      :  Frontend.Syntax.ParserTest
Description :  Tests for parser
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for the parser of DFL.
-}
module Frontend.Syntax.ParserTest where

import Data.Proxy
import Data.Void

import Test.Hspec

import Text.Megaparsec (ParseErrorBundle)

import Frontend.Syntax.Ast
import Frontend.Syntax.Parser
import Frontend.Syntax.Position (WithLocation(..), sourceLocation)
import Frontend.Syntax.Stream (TokenStream)
import Frontend.Syntax.Token
import Frontend.Syntax.Utils.AstExamples
import Frontend.Syntax.Utils.RandomSelector
import Frontend.Syntax.Utils.Tokenisable

testSuite :: IO ()
testSuite =
    hspec $ do
        describe "Basic parsers" $ do
            it "parses symbols" $
                shouldParseAllExamples (Proxy :: Proxy Special)
            it "parses keywords" $
                shouldParseAllExamples (Proxy :: Proxy Keyword)
            it "parses operators" $
                shouldParseAllExamples (Proxy :: Proxy Operator)
            it "parses integer" $ shouldParseAllExamples (Proxy :: Proxy IntT)
            it "parses float" $ shouldParseAllExamples (Proxy :: Proxy FloatT)
            it "parses char" $ shouldParseAllExamples (Proxy :: Proxy CharT)
            it "parses string" $ shouldParseAllExamples (Proxy :: Proxy StringT)
            it "parses QVarId" $ shouldParseAllExamples (Proxy :: Proxy QVarId)
            it "parses VarId" $ shouldParseAllExamples (Proxy :: Proxy VarId)
            it "parses TyVar" $ shouldParseAllExamples (Proxy :: Proxy TyVar)
            it "parses QConId" $ shouldParseAllExamples (Proxy :: Proxy QConId)
            it "parses ConId" $ shouldParseAllExamples (Proxy :: Proxy ConId)
            it "parses QTyCon" $ shouldParseAllExamples (Proxy :: Proxy QTyCon)
            it "parses TyCon" $ shouldParseAllExamples (Proxy :: Proxy TyCon)
            it "parses QTyCls" $ shouldParseAllExamples (Proxy :: Proxy QTyCls)
            it "parses TyCls" $ shouldParseAllExamples (Proxy :: Proxy TyCls)
            it "parses QVarSym" $
                shouldParseAllExamples (Proxy :: Proxy QVarSym)
            it "parses VarSym" $ shouldParseAllExamples (Proxy :: Proxy VarSym)
            it "parses minus" $
                minus `shouldParse`
                [defaultLocation $ TokenName [] (NameVarSym $ VarSym "-")] $
                VarSym "-"
            it "parses colon" $
                colon `shouldParse`
                [defaultLocation $ TokenName [] (NameConSym $ ConSym ":")] $
                ConSym ":"
            it "parses QConSym" $
                shouldParseAllExamples (Proxy :: Proxy QConSym)
            it "parses ConSym" $ shouldParseAllExamples (Proxy :: Proxy ConSym)
            it "parses QModId" $ shouldParseAllExamples (Proxy :: Proxy QModId)
            it "parses EOF" $ shouldParseAllExamples (Proxy :: Proxy EOF)
        describe "Composite parsers" $ do
            it "parses Literal" $ shouldParseAllExamples (Proxy :: Proxy Literal)
            it "parses GCon" $ shouldParseAllExamples (Proxy :: Proxy GCon)
            it "parses GTyCon" $ shouldParseAllExamples (Proxy :: Proxy GTyCon)
            it "parses Var" $ shouldParseAllExamples (Proxy :: Proxy Var)
            it "parses QVar" $ shouldParseAllExamples (Proxy :: Proxy QVar)
            it "parses Con" $ shouldParseAllExamples (Proxy :: Proxy Con)
            it "parses QCon" $ shouldParseAllExamples (Proxy :: Proxy QCon)
            it "parses VarOp" $ shouldParseAllExamples (Proxy :: Proxy VarOp)
            it "parses QVarOp" $ shouldParseAllExamples (Proxy :: Proxy QVarOp)
            it "parses ConOp" $ shouldParseAllExamples (Proxy :: Proxy ConOp)
            it "parses QConOp" $ shouldParseAllExamples (Proxy :: Proxy QConOp)
            it "parses Op" $ shouldParseAllExamples (Proxy :: Proxy Op)
            it "parses QOp" $ shouldParseAllExamples (Proxy :: Proxy QOp)
            it "parses GConSym" $
                shouldParseAllExamples (Proxy :: Proxy GConSym)
        describe "Location tracking" $
            it "tracks locations" $
            (parser :: Parser (WithLocation EOF)) `shouldParse`
            [WithLocation (TokenEOF EOF) (sourceLocation 1 2 1 3)] $
            WithLocation EOF (sourceLocation 1 2 1 3)
  where
    parseState ::
           Parser a
        -> [WithLocation Token]
        -> (Either (ParseErrorBundle TokenStream Void) a, ParserState)
    parseState parser' = runParser parser' ""
    parseSimple ::
           Parser a
        -> [WithLocation Token]
        -> Either (ParseErrorBundle TokenStream Void) a
    parseSimple parser' tokens = fst $ parseState parser' tokens
    shouldParse ::
           (Tokenisable a, Show a, Eq a)
        => Parser a
        -> [WithLocation Token]
        -> a
        -> Expectation
    shouldParse parser' tokens x = parseSimple parser' tokens `shouldBe` Right x
    shouldParseExamples ::
           (Show a, Eq a, Parseable a, Tokenisable a)
        => Proxy a
        -> [a]
        -> Expectation
    shouldParseExamples _ =
        mapM_
            (\x ->
                 shouldParse
                     (parser <* (parser :: Parser EOF))
                     (defaultLocation <$> (toTokens x ++ [TokenEOF EOF]))
                     x)
    shouldParseAllExamples ::
           (Show a, Eq a, Parseable a, Tokenisable a, WithExamples a)
        => Proxy a
        -> Expectation
    shouldParseAllExamples = shouldParseAllExamples' 10
    shouldParseAllExamples' ::
           (Show a, Eq a, Parseable a, Tokenisable a, WithExamples a)
        => Int
        -> Proxy a
        -> Expectation
    shouldParseAllExamples' n pxy =
        mapM_ (shouldParseExamples pxy) $ evalRandomSelector (getExamples n) 42
    defaultLocation :: a -> WithLocation a
    defaultLocation x = WithLocation x (sourceLocation 1 1 1 1) -- Fake location
