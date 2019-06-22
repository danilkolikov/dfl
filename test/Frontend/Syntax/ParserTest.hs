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

import Frontend.Syntax.Ast hiding (minus)
import Frontend.Syntax.Parser
import Frontend.Syntax.Position (WithLocation(..), sourceLocation)
import Frontend.Syntax.Stream (TokenStream)
import Frontend.Syntax.Token
import Frontend.Syntax.Utils.AstExamples (WithExamples(..))
import Frontend.Syntax.Utils.Tokenisable
import Frontend.Utils.RandomSelector

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
            it "parses Literal" $
                shouldParseAllExamples (Proxy :: Proxy Literal)
            it "parses Module" $ shouldParseAllExamples (Proxy :: Proxy Module)
            it "parses ImpExpList" $
                shouldParseAllExamples (Proxy :: Proxy ImpExpList)
            it "parses Export" $ shouldParseAllExamples (Proxy :: Proxy Export)
            it "parses Body" $ shouldParseAllExamples (Proxy :: Proxy Body)
            it "parses ImpDecl" $
                shouldParseAllExamples (Proxy :: Proxy ImpDecl)
            it "parses ImpSpec" $
                shouldParseAllExamples (Proxy :: Proxy ImpSpec)
            it "parses Import" $ shouldParseAllExamples (Proxy :: Proxy Import)
            it "parses CName" $ shouldParseAllExamples (Proxy :: Proxy CName)
            it "parses TopDecl" $
                shouldParseAllExamples (Proxy :: Proxy TopDecl)
            it "parses Decl" $ shouldParseAllExamples (Proxy :: Proxy Decl)
            it "parses CDecl" $ shouldParseAllExamples (Proxy :: Proxy CDecl)
            it "parses IDecl" $ shouldParseAllExamples (Proxy :: Proxy IDecl)
            it "parses GenDecl" $
                shouldParseAllExamples (Proxy :: Proxy GenDecl)
            it "parses Fixity" $ shouldParseAllExamples (Proxy :: Proxy Fixity)
            it "parses Type" $ shouldParseAllExamples (Proxy :: Proxy Type)
            it "parses BType" $ shouldParseAllExamples (Proxy :: Proxy BType)
            it "parses AType" $ shouldParseAllExamples (Proxy :: Proxy AType)
            it "parses GTyCon" $ shouldParseAllExamples (Proxy :: Proxy GTyCon)
            it "parses Class" $ shouldParseAllExamples (Proxy :: Proxy Class)
            it "parses SimpleClass" $
                shouldParseAllExamples (Proxy :: Proxy SimpleClass)
            it "parses SimpleType" $
                shouldParseAllExamples (Proxy :: Proxy SimpleType)
            it "parses Constr" $ shouldParseAllExamples (Proxy :: Proxy Constr)
            it "parses NewConstr" $
                shouldParseAllExamples (Proxy :: Proxy NewConstr)
            it "parses FieldDecl" $
                shouldParseAllExamples (Proxy :: Proxy FieldDecl)
            it "parses DClass" $ shouldParseAllExamples (Proxy :: Proxy DClass)
            it "parses Inst" $ shouldParseAllExamples (Proxy :: Proxy Inst)
            it "parses FunLHS" $ shouldParseAllExamples (Proxy :: Proxy FunLHS)
            it "parses RHS" $ shouldParseAllExamples (Proxy :: Proxy RHS)
            it "parses GdRHS" $ shouldParseAllExamples (Proxy :: Proxy GdRHS)
            it "parses Guard" $ shouldParseAllExamples (Proxy :: Proxy Guard)
            it "parses Exp" $ shouldParseAllExamples (Proxy :: Proxy Exp)
            it "parses InfixExp" $
                shouldParseAllExamples (Proxy :: Proxy InfixExp)
            it "parses LExp" $ shouldParseAllExamples (Proxy :: Proxy LExp)
            it "parses AExp" $ shouldParseAllExamples (Proxy :: Proxy AExp)
            it "parses Qual" $ shouldParseAllExamples (Proxy :: Proxy Qual)
            it "parses Alt" $ shouldParseAllExamples (Proxy :: Proxy Alt)
            it "parses GdPat" $ shouldParseAllExamples (Proxy :: Proxy GdPat)
            it "parses Stmt" $ shouldParseAllExamples (Proxy :: Proxy Stmt)
            it "parses FBind" $ shouldParseAllExamples (Proxy :: Proxy FBind)
            it "parses Pat" $ shouldParseAllExamples (Proxy :: Proxy Pat)
            it "parses LPat" $ shouldParseAllExamples (Proxy :: Proxy LPat)
            it "parses APat" $ shouldParseAllExamples (Proxy :: Proxy APat)
            it "parses FPat" $ shouldParseAllExamples (Proxy :: Proxy FPat)
            it "parses GCon" $ shouldParseAllExamples (Proxy :: Proxy GCon)
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
            it "tracks locations" $ do
                (parser :: Parser (WithLocation EOF)) `shouldParse`
                    [WithLocation (TokenEOF EOF) (sourceLocation 1 2 1 3)] $
                    WithLocation EOF (sourceLocation 1 2 1 3)
                (parser :: Parser (WithLocation ImpSpec)) `shouldParse`
                    [ WithLocation
                          (TokenName [] (NameVarId (VarId "hiding")))
                          (sourceLocation 1 1 1 7)
                    , WithLocation
                          (TokenSpecial SpecialLParen)
                          (sourceLocation 1 8 1 9)
                    , WithLocation
                          (TokenName [] (NameVarId (VarId "foo")))
                          (sourceLocation 2 4 2 7)
                    , WithLocation
                          (TokenSpecial SpecialComma)
                          (sourceLocation 2 7 2 8)
                    , WithLocation
                          (TokenName [] (NameConId (ConId "Data")))
                          (sourceLocation 3 4 3 8)
                    , WithLocation
                          (TokenSpecial SpecialLParen)
                          (sourceLocation 3 8 3 9)
                    , WithLocation
                          (TokenOperator OperatorDDot)
                          (sourceLocation 3 9 3 11)
                    , WithLocation
                          (TokenSpecial SpecialRParen)
                          (sourceLocation 3 11 3 12)
                    , WithLocation
                          (TokenSpecial SpecialRParen)
                          (sourceLocation 3 12 3 13)
                    ] $
                    WithLocation
                        (ImpSpec
                             True
                             [ WithLocation
                                   (ImportFunction
                                        (WithLocation
                                             (FuncLabelId $ VarId "foo")
                                             (sourceLocation 2 4 2 7)))
                                   (sourceLocation 2 4 2 7)
                             , WithLocation
                                   (ImportDataOrClass
                                        (WithLocation
                                             (ConId "Data")
                                             (sourceLocation 3 4 3 8))
                                        (WithLocation
                                             ImpExpAll
                                             (sourceLocation 3 8 3 12)))
                                   (sourceLocation 3 4 3 12)
                             ])
                        (sourceLocation 1 1 3 13)
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
        mapM_ (shouldParseExamples pxy) $
        evalRandomSelector (getExamples n) 42 3
    defaultLocation :: a -> WithLocation a
    defaultLocation x = WithLocation x (sourceLocation 1 1 1 1) -- Fake location
