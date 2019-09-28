{- |
Module      :  Frontend.Syntax.Processor
Description :  Syntax Analyser of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Syntax analyser of DFL. Parses source files and resolves fixity.
-}
module Frontend.Syntax.Processor
    ( SyntaxProcessorError(..)
    , SyntaxProcessorOutput(..)
    , SyntaxProcessorDebugOutput(..)
    , processModuleSyntax
    , ParserError(..)
    , AstCheckerError(..)
    , FixityResolutionError(..)
    , InfixOperator(..)
    , InfixOperators
    , TokenStream(..)
    , Module
    , Body
    ) where

import Control.Applicative ((<|>))

import Frontend.Syntax.Ast (Body, Module)
import Frontend.Syntax.AstChecker (AstCheckerError(..), checkAst)
import Frontend.Syntax.FixityResolution
    ( FixityResolutionError(..)
    , InfixOperator(..)
    , InfixOperators
    , resolveModuleFixity
    )
import Frontend.Syntax.Parser (ParserError(..), parseModule)
import Frontend.Syntax.Stream (TokenStream(..))
import Util.Debug

-- | Errors which can be encountered during syntax analysis
data SyntaxProcessorError
    = SyntaxProcessorErrorParser ParserError -- ^ Parsing error
    | SyntaxProcessorErrorChecker AstCheckerError -- ^ Error caused by wrong AST-s
    | SyntaxProcessorErrorFixity FixityResolutionError -- ^ Fixity resolution error
    deriving (Eq, Show)

-- | Output of the syntax processor
data SyntaxProcessorOutput = SyntaxProcessorOutput
    { getSyntaxProcessorOutputAst :: Module Body
    , getSyntaxProcessorOutputInfix :: InfixOperators
    } deriving (Eq, Show)

-- | Debug output of the syntax processor
data SyntaxProcessorDebugOutput = SyntaxProcessorDebugOutput
    { getSyntaxProcessorDebugOutputInitialAst :: Maybe (Module Body)
    , getSyntaxProcessorDebugOutputResolvedAst :: Maybe (Module Body)
    , getSyntaxProcessorDebugOutputInfix :: Maybe InfixOperators
    } deriving (Eq, Show)

instance Semigroup SyntaxProcessorDebugOutput where
    SyntaxProcessorDebugOutput a1 r1 i1 <> SyntaxProcessorDebugOutput a2 r2 i2 =
        SyntaxProcessorDebugOutput (a1 <|> a2) (r1 <|> r2) (i1 <|> i2)

instance Monoid SyntaxProcessorDebugOutput where
    mempty = SyntaxProcessorDebugOutput Nothing Nothing Nothing

-- | Processes syntax of the module - parses, checks correctness and resolves
--   fixity of operators
processModuleSyntax ::
       InfixOperators
    -> String
    -> TokenStream
    -> ( Either SyntaxProcessorError SyntaxProcessorOutput
       , SyntaxProcessorDebugOutput)
processModuleSyntax initialInfixOperators fileName stream =
    runWithDebugOutput $ do
        ast <-
            wrapEither SyntaxProcessorErrorParser $ parseModule fileName stream
        writeDebugOutput
            mempty {getSyntaxProcessorDebugOutputInitialAst = Just ast}
        _ <- wrapEither SyntaxProcessorErrorChecker $ checkAst ast
        (resolvedAst, infixOperators) <-
            wrapEither SyntaxProcessorErrorFixity $
            resolveModuleFixity initialInfixOperators ast
        writeDebugOutput
            mempty
                { getSyntaxProcessorDebugOutputResolvedAst = Just resolvedAst
                , getSyntaxProcessorDebugOutputInfix = Just infixOperators
                }
        return
            SyntaxProcessorOutput
                { getSyntaxProcessorOutputAst = resolvedAst
                , getSyntaxProcessorOutputInfix = infixOperators
                }
