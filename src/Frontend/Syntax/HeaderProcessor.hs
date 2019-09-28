{- |
Module      :  Frontend.Syntax.HeaderProcessor
Description :  Lexical Analyser of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Lexical analyser of DFL. Does layout-based lexing and parses header of a module.
-}
module Frontend.Syntax.HeaderProcessor
    ( HeaderProcessorError(..)
    , HeaderProcessorOutput(..)
    , HeaderProcessorDebugOutput(..)
    , processModuleHeader
    , ParserError(..)
    , LexerError(..)
    , TokenStream(..)
    , Module
    , Header
    ) where

import Control.Applicative ((<|>))

import Frontend.Syntax.Ast (Header, Module)
import Frontend.Syntax.Lexer (LexerError(..), runLexer)
import Frontend.Syntax.Parser (ParserError(..), parseHeader)
import Frontend.Syntax.Stream (TokenStream(..))
import Util.Debug

-- | Errors which can be encountered during processing
data HeaderProcessorError
    = HeaderProcessorErrorLexer LexerError -- ^ Lexing error
    | HeaderProcessorErrorParser ParserError -- ^ Parsing error
    deriving (Eq, Show)

-- | Output of the processor
data HeaderProcessorOutput = HeaderProcessorOutput
    { getHeaderProcessorOutputTokens :: TokenStream
    , getHeaderProcessorOutputHeader :: Module Header
    } deriving (Eq, Show)

-- | Debug output of the syntax processor
data HeaderProcessorDebugOutput = HeaderProcessorDebugOutput
    { getHeaderProcessorDebugOutputTokens :: Maybe TokenStream
    , getHeaderProcessorDebugOutputHeader :: Maybe (Module Header)
    } deriving (Eq, Show)

instance Semigroup HeaderProcessorDebugOutput where
    HeaderProcessorDebugOutput t1 h1 <> HeaderProcessorDebugOutput t2 h2 =
        HeaderProcessorDebugOutput (t1 <|> t2) (h1 <|> h2)

instance Monoid HeaderProcessorDebugOutput where
    mempty = HeaderProcessorDebugOutput Nothing Nothing

-- | Processes syntax of the module - does lexing and parses a header
processModuleHeader ::
       String
    -> String
    -> ( Either HeaderProcessorError HeaderProcessorOutput
       , HeaderProcessorDebugOutput)
processModuleHeader fileName content =
    runWithDebugOutput $ do
        tokens <-
            wrapEither HeaderProcessorErrorLexer $
            runLexer fileName content
        let stream = TokenStream tokens
        writeDebugOutput
            mempty {getHeaderProcessorDebugOutputTokens = Just stream}
        header <-
            wrapEither HeaderProcessorErrorParser $ parseHeader fileName stream
        writeDebugOutput
            mempty {getHeaderProcessorDebugOutputHeader = Just header}
        return
            HeaderProcessorOutput
                { getHeaderProcessorOutputTokens = stream
                , getHeaderProcessorOutputHeader = header
                }
