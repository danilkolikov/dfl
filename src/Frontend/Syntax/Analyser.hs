{- |
Module      :  Frontend.Syntax.Analyser
Description :  Syntax Analyser of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Syntax analyser of DFL. Does layout-based lexing and parsing.
-}
module Frontend.Syntax.Analyser
    ( AnalyserState(..)
    , OperatorName
    , InfixOperator(..)
    , SyntaxError(..)
    , LayoutError(..)
    , AstCheckerError(..)
    , FixityResolutionError(..)
    , runSyntaxAnalyser
    ) where

import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Except as E (Except, except, runExcept)
import qualified Control.Monad.Trans.State as ST
    ( StateT
    , get
    , modify
    , runStateT
    )
import Data.Bifunctor (first)
import qualified Data.HashMap.Lazy as HM
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

import Frontend.Syntax.Ast (Module)
import Frontend.Syntax.AstChecker (AstCheckerError(..), checkAst)
import Frontend.Syntax.FixityResolution
    ( FixityResolutionError(..)
    , InfixOperator(..)
    , OperatorName
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

-- | State of the syntax analyser
data AnalyserState = AnalyserState
    { getFileName :: String -- ^ File name
    , getInfixOperators :: HM.HashMap OperatorName InfixOperator -- ^ Map of infix operators
    } deriving (Show, Eq)

-- | Errors which can be encountered during syntax analysis
data SyntaxError
    = SyntaxErrorLexer SourcePosition
                       (Maybe Char)
                       [String] -- ^ Lexer error
    | SyntaxErrorLayout LayoutError -- ^ Error caused by wrong file layout
    | SyntaxErrorParser SourceLocation
                        (Maybe Token)
                        [String] -- ^ Parser error
    | SyntaxErrorChecker AstCheckerError -- ^ Error caused by wrong AST-s
    | SyntaxErrorFixity FixityResolutionError -- ^ Error caused by error in fixity resolution
    | SyntaxErrorUnexpected SourcePosition -- ^ Unknown error
    deriving (Show, Eq)

-- | Type of the syntax analyser
type SyntaxAnalyser a = ST.StateT AnalyserState (E.Except SyntaxError) a

-- | Run syntax analyser on the specified code
runSyntaxAnalyser ::
       String -> AnalyserState -> Either SyntaxError (Module, AnalyserState)
runSyntaxAnalyser input st =
    E.runExcept (ST.runStateT (syntaxAnalyser input) st)

-- | Syntax analyser
syntaxAnalyser :: String -> SyntaxAnalyser Module
syntaxAnalyser input = do
    state <- ST.get
    let fileName = getFileName state
        infixOperators = getInfixOperators state
    -- Run lexer
    let lexerOutput = wrapLexerError $ runParser sourceLexer fileName input
    lexems <- except lexerOutput
    -- Run parser
    let parserOutput =
            wrapParserError lexems $ runParser' sourceParser fileName lexems
    ast <- except parserOutput
    -- Check AST
    let checked = wrapAstCheckerError $ checkAst ast
    except checked
    -- Resolve fixity
    let resolvedFixity =
            wrapFixityError $
            runFixityResolver (fixityResolver ast) [infixOperators]
    resolved <- except resolvedFixity
    -- Update state and return result
    let (result, newOperators) = resolved
    ST.modify $ \st -> st {getInfixOperators = head newOperators}
    return result
  where
    except = lift . E.except
    sourceParser = (parser :: Parser Module) <* (parser :: Parser EOF)

-- Helper functions
wrapLexerError ::
       Either (ParseErrorBundle String LayoutError) a -> Either SyntaxError a
wrapLexerError = first wrapBundle
  where
    wrapBundle :: ParseErrorBundle String LayoutError -> SyntaxError
    wrapBundle bundle =
        let firstError = NE.head . bundleErrors $ bundle
            position =
                castSourcePosition . pstateSourcePos . bundlePosState $ bundle
         in case firstError of
                TrivialError _ got expected ->
                    SyntaxErrorLexer
                        position
                        (getTokens' <$> got)
                        (getLabel <$> S.toList expected)
                FancyError _ errors ->
                    case S.findMin errors of
                        ErrorCustom layout -> SyntaxErrorLayout layout
                        _ -> SyntaxErrorUnexpected position

wrapParserError ::
       [WithLocation Token]
    -> Either (ParseErrorBundle TokenStream Void) a
    -> Either SyntaxError a
wrapParserError tokens = first wrapBundle
  where
    wrapBundle :: ParseErrorBundle TokenStream Void -> SyntaxError
    wrapBundle bundle =
        let firstError = NE.head . bundleErrors $ bundle
         in case firstError of
                TrivialError pos got expected ->
                    SyntaxErrorParser
                        (getLocation $ tokens !! pos)
                        (getValue . getTokens' <$> got)
                        (getLabel <$> S.toList expected)
                FancyError pos _ ->
                    SyntaxErrorUnexpected
                        (getLocationStart . getLocation $ tokens !! pos)

wrapAstCheckerError :: Either AstCheckerError () -> Either SyntaxError ()
wrapAstCheckerError = first SyntaxErrorChecker

wrapFixityError :: Either FixityResolutionError a -> Either SyntaxError a
wrapFixityError = first SyntaxErrorFixity

getTokens' :: ErrorItem a -> a
getTokens' (Tokens ts) = NE.head ts
getTokens' _ = undefined

getLabel :: ErrorItem a -> String
getLabel (Label chars) = NE.toList chars
getLabel EndOfInput = "End Of Input"
getLabel _ = ""
