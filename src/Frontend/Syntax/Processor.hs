{- |
Module      :  Frontend.Syntax.Processor
Description :  Syntax Analyser of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Syntax analyser of DFL. Parses source files and resolves fixity.
-}
module Frontend.Syntax.Processor
    ( SyntaxProcessorError(..)
    , SyntaxProcessorOutput
    , SyntaxProcessorDebugOutput(..)
    , processModuleSyntax
    , ParserError(..)
    , AstCheckerError(..)
    , TokenStream(..)
    , Module
    , Body
    ) where

import Control.Applicative ((<|>))

import Frontend.Syntax.Ast (Body, Module)
import Frontend.Syntax.AstChecker (AstCheckerError(..), checkAst)
import Frontend.Syntax.Parser (ParserError(..), parseModule)
import Frontend.Syntax.Stream (TokenStream(..))
import Util.Debug

-- | Errors which can be encountered during syntax analysis
data SyntaxProcessorError
    = SyntaxProcessorErrorParser ParserError -- ^ Parsing error
    | SyntaxProcessorErrorChecker AstCheckerError -- ^ Error caused by wrong AST-s
    deriving (Eq, Show)

-- | Output of the syntax processor
type SyntaxProcessorOutput = Module Body

-- | Debug output of the syntax processor
newtype SyntaxProcessorDebugOutput = SyntaxProcessorDebugOutput
    { getSyntaxProcessorDebugOutputAst :: Maybe (Module Body)
    } deriving (Eq, Show)

instance Semigroup SyntaxProcessorDebugOutput where
    SyntaxProcessorDebugOutput a1 <> SyntaxProcessorDebugOutput a2 =
        SyntaxProcessorDebugOutput (a1 <|> a2)

instance Monoid SyntaxProcessorDebugOutput where
    mempty = SyntaxProcessorDebugOutput Nothing

-- | Processes syntax of the module - parses, checks correctness and resolves
--   fixity of operators
processModuleSyntax ::
       String
    -> TokenStream
    -> ( Either SyntaxProcessorError SyntaxProcessorOutput
       , SyntaxProcessorDebugOutput)
processModuleSyntax fileName stream =
    runWithDebugOutput $ do
        ast <-
            wrapEither SyntaxProcessorErrorParser $ parseModule fileName stream
        writeDebugOutput mempty {getSyntaxProcessorDebugOutputAst = Just ast}
        _ <- wrapEither SyntaxProcessorErrorChecker $ checkAst ast
        return ast
