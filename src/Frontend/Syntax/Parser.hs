{- |
Module      :  Frontend.Syntax.Parser
Description :  Parser for DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Parser for DFL.
-}
module Frontend.Syntax.Parser
    ( Parser
    , ParserState
    , Parseable(..)
    , runParser
    , minus
    , colon
    ) where

import Control.Monad (guard, when)
import Control.Monad.Combinators.NonEmpty (some)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as ST (State, get, put, runState)
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec
    ( ParseErrorBundle
    , ParsecT
    , State(..)
    , (<?>)
    , between
    , choice
    , empty
    , getParserState
    , optional
    , runParserT
    , token
    , try
    )

import Frontend.Syntax.Ast
import Frontend.Syntax.Position
    ( SourceLocation(..)
    , WithLocation(..)
    , dummyLocation
    )
import Frontend.Syntax.Stream (TokenStream(..))
import Frontend.Syntax.Token

-- | State of the parser. State contains location of the last parsed token
type ParserState = Maybe SourceLocation

-- | Parser of DFL
type Parser = ParsecT Void TokenStream (ST.State ParserState)

-- | Function that runs parser
runParser ::
       Parser a -- ^ Parser to run
    -> String -- ^ File name
    -> [WithLocation Token] -- ^ List of tokens, returned by a lexer
    -> (Either (ParseErrorBundle TokenStream Void) a, ParserState)
runParser parser' str tokens =
    ST.runState (runParserT parser' str (TokenStream tokens)) Nothing

-- | Class of types which can be parsed from the list of tokens
class Parseable a where
    parser :: Parser a -- ^ Parser of objects

instance Parseable Special where
    parser = parserForTokenContains <?> "special symbol"

instance Parseable Keyword where
    parser = parserForTokenContains <?> "keyword"

instance Parseable Operator where
    parser = parserForTokenContains <?> "operator"

instance Parseable IntT where
    parser = parserForTokenContains <?> "Integer number"

instance Parseable FloatT where
    parser = parserForTokenContains <?> "Float number"

instance Parseable CharT where
    parser = parserForTokenContains <?> "Character"

instance Parseable StringT where
    parser = parserForTokenContains <?> "String"

instance Parseable NameWithPath where
    parser = parserForTokenContains

instance Parseable VarId where
    parser = parseNotQualified <?> "Lowercase name"

instance Parseable ConId where
    parser = parseNotQualified <?> "Uppercase name"

instance Parseable VarSym where
    parser = parseNotQualified <?> "Variable operator"

-- | Parser for the operator "-"
minus :: Parser VarSym
minus = expect (VarSym "-")

instance Parseable ConSym where
    parser = parseNotQualified <?> "Constructor operator"

-- | Parser for the operator ":"
colon :: Parser ConSym
colon = expect (ConSym ":")

instance (NameContains a) => Parseable (Qualified a) where
    parser =
        parser >>= \(NameWithPath path name) ->
            case getFromName name of
                Just res -> return $ Qualified path res
                Nothing -> empty

instance Parseable EOF where
    parser = parserForTokenContains <?> "EOF"

instance (Parseable a, Parseable b) => Parseable (Either a b) where
    parser = safeChoice [Left <$> parser, Right <$> parser]

instance (Parseable a) => Parseable (WithLocation a) where
    parser = do
        nextTokenLocation <- getNextTokenLocation -- Get location of the next token
        let (SourceLocation start _) = fromMaybe dummyLocation nextTokenLocation
        -- Run parser
        value <- parser
        -- Get location of the last parsed token from state
        state <- lift ST.get
        let (SourceLocation _ end) = fromMaybe dummyLocation state
        return $ WithLocation value (SourceLocation start end)
      where
        getNextTokenLocation :: Parser (Maybe SourceLocation)
        getNextTokenLocation =
            getParserState >>=
            (\(State input _ _) ->
                 return $
                 -- Find the first non-implicit token and return its location
                 find (/= dummyLocation) (map getLocation $ getTokens input))

instance Parseable Literal where
    parser =
        safeChoice
            [ LiteralInteger <$> parser
            , LiteralFloat <$> parser
            , LiteralChar <$> parser
            , LiteralString <$> parser
            ]

instance Parseable GCon where
    parser =
        safeChoice
            [ inParens $ do
                  commas <- safeOptional $ some (try $ expect SpecialComma)
                  return $
                      case commas of
                          Just cmms -> GConTuple $ NE.length cmms + 1
                          Nothing -> GConUnit
            , inBrackets $ return GConList
            , liftP1 GConNamed
            ]

instance Parseable GTyCon where
    parser =
        safeChoice
            [ liftP1 GTyConNamed
            , inParens $
              safeChoice
                  [ GTyConFunction <$ expect OperatorRArrow
                  , GTyConTuple . (+ 1) . NE.length <$>
                    some (try $ expect SpecialComma)
                  , return GTyConUnit
                  ]
            , inBrackets $ return GTyConList
            ]

instance (Parseable a, Parseable b) => Parseable (FuncLabel a b) where
    parser = safeChoice [inParens $ liftP1 FuncLabelSym, liftP1 FuncLabelId]

instance (Parseable a, Parseable b) => Parseable (OpLabel a b) where
    parser = safeChoice [inBackticks $ liftP1 OpLabelId, liftP1 OpLabelSym]

instance Parseable GConSym where
    parser = safeChoice [GConSymColon <$ colon, liftP1 GConSymOp]

-- Helper functions
-- | Try every parser and choose the first succeeded one.
safeChoice :: [Parser a] -> Parser a
safeChoice = choice . map try

-- | Try to run a parser and reset back, if the parser fails
safeOptional :: Parser a -> Parser (Maybe a)
safeOptional = optional . try

-- | "Wrap" the parser in parenthesis
inParens :: Parser a -> Parser a
inParens = between (expect SpecialLParen) (expect SpecialRParen)

-- | "Wrap" the parser in square brackets
inBrackets :: Parser a -> Parser a
inBrackets = between (expect SpecialLBracket) (expect SpecialRBracket)

-- | "Wrap" the parser in backticks
inBackticks :: Parser a -> Parser a
inBackticks = between (expect SpecialBacktick) (expect SpecialBacktick)

-- | Run parser and pass result as an argument to a function
liftP1 :: (Parseable a) => (a -> b) -> Parser b
liftP1 f = f <$> parser

-- | Parser for types from TokenContains type class
--   This is the most low-level parser. After a token is parsed,
--   we update location of the last parsed token in the state
parserForTokenContains :: (TokenContains a) => Parser a
parserForTokenContains = do
    (WithLocation res loc) <- parserForToken
    -- Update state, if parsed token is not an implicit one
    when (loc /= dummyLocation) $ lift . ST.put $ Just loc
    return res
  where
    parserForToken :: (TokenContains a) => Parser (WithLocation a)
    parserForToken = token (traverse getFromToken) S.empty

-- | Parse not-qualified names
parseNotQualified :: (NameContains a) => Parser a
parseNotQualified =
    parser >>= \(NameWithPath path name) ->
        guard (null path) >>
        case getFromName name of
            Just res -> return res
            Nothing -> empty

-- | Expect the next parsed object to be equal to the provided one
expect :: (Parseable a, Eq a, Show a) => a -> Parser a
expect x =
    (parser >>= \y ->
         if x == y
             then return y
             else empty) <?>
    show x