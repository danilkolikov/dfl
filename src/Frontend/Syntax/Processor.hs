{- |
Module      :  Frontend.Syntax.Processor
Description :  Syntax Analyser of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Syntax analyser of DFL. Does layout-based lexing and parsing.
-}
module Frontend.Syntax.Processor
    ( LexicalError(..)
    , LayoutError(..)
    , ParserError(..)
    , AstCheckerError(..)
    , FixityResolutionError(..)
    , lexicalAnalysis
    , parsing
    , parseHeader
    , InfixOperator(..)
    , InfixOperators
    , FixityResolutionOutput(..)
    , fixityResolution
    , TokenStream(..)
    , Module
    , Body
    , Header
    ) where

import Data.Bifunctor (bimap, first)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Data.Void
import Text.Megaparsec
    ( ErrorFancy(..)
    , ErrorItem(..)
    , ParseError(..)
    , ParseErrorBundle(..)
    , PosState(..)
    , runParser
    )

import Frontend.Syntax.Ast (Body, Header, Module)
import Frontend.Syntax.AstChecker (AstCheckerError(..), checkAst)
import Frontend.Syntax.FixityResolution
    ( FixityResolutionError(..)
    , InfixOperator(..)
    , InfixOperators
    , fixityResolver
    , runFixityResolver
    )
import Frontend.Syntax.Layout (LayoutError(..))
import Frontend.Syntax.Lexer (sourceLexer)
import Frontend.Syntax.Parser (Parser, parser, runParser')
import Frontend.Syntax.Position
    ( SourceLocation(..)
    , SourcePosition(..)
    , WithLocation(..)
    , castSourcePosition
    )
import Frontend.Syntax.Stream (TokenStream(..))
import Frontend.Syntax.Token (EOF, Token)

-- | Errors which can be encountered during syntax analysis
data LexicalError
    = LexicalErrorLexer SourcePosition
                        (Maybe Char)
                        [String] -- ^ Lexer error
    | LexicalErrorLayout LayoutError -- ^ Error caused by wrong file layout
    | LexicalErrorUnexpected SourcePosition -- ^ Unknown error from the parser's machinery
    deriving (Show, Eq)

-- | Do lexical analysis
lexicalAnalysis :: String -> String -> Either LexicalError TokenStream
lexicalAnalysis fileName fileContent =
    TokenStream <$> wrapLexerError (runParser sourceLexer fileName fileContent)
  where
    wrapLexerError = first wrapLexicalBundle
    wrapLexicalBundle :: ParseErrorBundle String LayoutError -> LexicalError
    wrapLexicalBundle bundle =
        let firstError = NE.head . bundleErrors $ bundle
            position =
                castSourcePosition . pstateSourcePos . bundlePosState $ bundle
         in case firstError of
                TrivialError _ got expected ->
                    LexicalErrorLexer
                        position
                        (getTokens' <$> got)
                        (getLabel <$> S.toList expected)
                FancyError _ errors ->
                    case S.findMin errors of
                        ErrorCustom layout -> LexicalErrorLayout layout
                        _ -> LexicalErrorUnexpected position

-- | Errors which can be encountered during syntax analysis
data ParserError
    = ParserErrorParser SourceLocation
                        (Maybe Token)
                        [String] -- ^ Parser error
    | ParserErrorChecker AstCheckerError -- ^ Error caused by wrong AST-s
    | ParserErrorUnexpected SourcePosition -- ^ Unknown error from the parser's machinery
    deriving (Show, Eq)

-- | Parses header of a module
parseHeader :: String -> TokenStream -> Either ParserError (Module Header)
parseHeader fileName stream =
    let sourceParser = (parser :: Parser (Module Header))
     in wrapParserError stream $
        runParser' sourceParser fileName (getTokens stream)

-- | Do parsing of tokens
parsing :: String -> TokenStream -> Either ParserError (Module Body)
parsing fileName stream = do
    let sourceParser =
            (parser :: Parser (Module Body)) <* (parser :: Parser EOF)
    ast <-
        wrapParserError stream $
        runParser' sourceParser fileName (getTokens stream)
    _ <- wrapAstCheckerError $ checkAst ast
    return ast
  where
    wrapAstCheckerError = first ParserErrorChecker

-- | Result of fixity resolution
data FixityResolutionOutput = FixityResolutionOutput
    { getFixityResolutionOutputAst :: Module Body -- ^ Resolved AST
    , getFixityResolutionOutputOperators :: InfixOperators -- ^ Map of infix operators
    } deriving (Eq, Show)

-- | Resolve fixity of infix operators
fixityResolution ::
       InfixOperators
    -> Module Body
    -> Either FixityResolutionError FixityResolutionOutput
fixityResolution infixOperators ast =
    wrapToOutput <$> runFixityResolver (fixityResolver ast) [infixOperators]
  where
    wrapToOutput = uncurry FixityResolutionOutput . bimap id head

-- Helper functions
-- | Get tokens from an ErrorItem
getTokens' :: ErrorItem a -> a
getTokens' (Tokens ts) = NE.head ts
getTokens' _ = undefined

-- | Get label from an ErrorItem
getLabel :: (Show a) => ErrorItem a -> String
getLabel (Label chars) = NE.toList chars
getLabel (Tokens ts) = concatMap show ts
getLabel EndOfInput = "End Of Input"

wrapParserError ::
       TokenStream
    -> Either (ParseErrorBundle TokenStream Void) a
    -> Either ParserError a
wrapParserError tokens = first (wrapParserBundle tokens)

wrapParserBundle ::
       TokenStream -> ParseErrorBundle TokenStream Void -> ParserError
wrapParserBundle (TokenStream tokens) bundle =
    let firstError = NE.head . bundleErrors $ bundle
     in case firstError of
            TrivialError pos got expected ->
                ParserErrorParser
                    (getLocation $ tokens !! pos)
                    (getValue . getTokens' <$> got)
                    (getLabel <$> S.toList expected)
            FancyError pos _ ->
                ParserErrorUnexpected
                    (getLocationStart . getLocation $ tokens !! pos)
