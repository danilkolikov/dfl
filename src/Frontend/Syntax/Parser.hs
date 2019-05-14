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

import Control.Applicative (liftA2)
import Control.Monad (guard, void, when)
import Control.Monad.Combinators (many, sepEndBy)
import Control.Monad.Combinators.NonEmpty (sepEndBy1, some)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as ST (State, get, put, runState)
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, isJust)
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
    parser = parseWithLocation parser

instance Parseable Literal where
    parser =
        safeChoice
            [ LiteralInteger <$> parser
            , LiteralFloat <$> parser
            , LiteralChar <$> parser
            , LiteralString <$> parser
            ]

instance Parseable Module where
    parser =
        safeChoice
            [ do expect_ KeywordModule
                 modId <- parser
                 exports <-
                     safeOptional $ inParens $ parser `sepByP` SpecialComma
                 expect_ KeywordWhere
                 body <- parser
                 return $ ModuleExplicit modId exports body
            , liftP1 ModuleImplicit
            ]

instance Parseable Export where
    parser =
        safeChoice
            [ liftP1 ExportFunction
            , liftP2 ExportDataOrClass
            , expect KeywordModule *> liftP1 ExportModule
            ]

instance Parseable ImpExpList where
    parser =
        fromMaybe ImpExpNothing <$>
        safeOptional
            (inParens $
             safeChoice
                 [ ImpExpAll <$ expect OperatorDDot
                 , ImpExpSome <$> parser `sepBy1P` SpecialComma
                 , return ImpExpNothing
                 ])

instance Parseable Body where
    parser =
        inCurly $
        safeChoice
            [ do impDecls <- parseImpDecls
                 return $ Body impDecls
            , return $ Body []
            ]
      where
        parseImpDecls :: Parser [WithLocation ImpDecl]
        parseImpDecls =
            NE.toList <$>
            try parser `sepEndBy1` try (some $ expect SpecialSemicolon)

instance Parseable ImpDecl where
    parser = do
        expect_ KeywordImport
        qual <- safeOptional $ expect (VarId "qualified")
        name <- parser
        asModId <- safeOptional $ expect (VarId "as") *> parser
        impSpec <- safeOptional parser
        return $ ImpDecl (isJust qual) name asModId impSpec

instance Parseable ImpSpec where
    parser = do
        hiding <- safeOptional $ expect (VarId "hiding")
        imports <- inParens $ parser `sepByP` SpecialComma
        return $ ImpSpec (isJust hiding) imports

instance Parseable Import where
    parser = safeChoice [liftP1 ImportFunction, liftP2 ImportDataOrClass]

instance Parseable Type where
    parser = Type <$> parser `sepBy1P` OperatorRArrow

instance Parseable BType where
    parser = BType <$> some parser

instance Parseable AType where
    parser =
        safeChoice
            [ liftP1 ATypeConstructor
            , liftP1 ATypeVar
            , inParens $ do
                  f <- parser
                  safeChoice
                      [ do expect_ SpecialComma
                           (s NE.:| rest) <- parser `sepBy1P` SpecialComma
                           return $ ATypeTuple f s rest
                      , return $ ATypeParens f
                      ]
            , ATypeList <$> inBrackets parser
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

instance Parseable Class where
    parser = do
        name <- parser
        safeChoice
            [ liftP1 (ClassSimple name)
            , inParens $ do
                  var <- parser
                  types <- some parser
                  return $ ClassApplied name var types
            ]

instance Parseable Exp where
    parser = do
        e <- parser
        safeChoice
            [ expect OperatorQDot *> liftA2 (ExpTyped e) parseContext parser
            , return $ ExpSimple e
            ]

instance Parseable InfixExp where
    parser = parseInfix parseSingle InfixExpApplication
      where
        parseSingle :: Parser InfixExp
        parseSingle =
            safeChoice [minus *> liftP1 InfixExpNegated, liftP1 InfixExpLExp]

instance Parseable LExp where
    parser =
        safeChoice
            [ do expect_ OperatorBackslash
                 args <- some parser
                 expect_ OperatorRArrow
                 e <- parser
                 return $ LExpAbstraction args e
            , do expect_ KeywordIf
                 cond <- parser
                 _ <- safeOptional $ expect SpecialSemicolon
                 expect_ KeywordThen
                 true <- parser
                 _ <- safeOptional $ expect SpecialSemicolon
                 expect_ KeywordElse
                 false <- parser
                 return $ LExpIf cond true false
            , LExpApplication <$> some parser
            ]

instance Parseable AExp where
    parser = do
        aexp <- parseWithLocation parseAExp
        fbinds <- many (try parseFBind)
        let recordUpdate = foldl createRecordUpdate aexp fbinds
        return $ getValue recordUpdate
      where
        parseAExp :: Parser AExp
        parseAExp =
            safeChoice
                [ liftP1 AExpVariable
                , liftP1 AExpLiteral
                , do con <- parser
                     fbinds <- inCurly $ parser `sepByP` SpecialComma
                     return $ AExpRecordConstr con fbinds
                , liftP1 AExpConstructor
                , inParens $
                  safeChoice
                      [ do f <- parser
                           expect_ SpecialComma
                           (s NE.:| rest) <- parser `sepBy1P` SpecialComma
                           return $ AExpTuple f s rest
                      , do op <- parser
                           guard $
                               getValue op /=
                               Left (OpLabelSym (Qualified [] $ VarSym "-"))
                           e <- parser
                           return $ AExpRightSection op e
                      , liftP2 AExpLeftSection
                      , liftP1 AExpParens
                      ]
                , inBrackets $ do
                      first <- parser
                      safeChoice
                          [ do expect_ SpecialComma
                               second <- parser
                               safeChoice
                                   [ do expect_ OperatorDDot
                                        end <- safeOptional parser
                                        return $
                                            AExpSequence first (Just second) end
                                   , do expect_ SpecialComma
                                        rest <- parser `sepByP` SpecialComma
                                        return $
                                            AExpList
                                                (first NE.:| (second : rest))
                                   , return $ AExpList (first NE.:| [second])
                                   ]
                          , do expect_ OperatorDDot
                               end <- safeOptional parser
                               return $ AExpSequence first Nothing end
                          , return $ AExpList (first NE.:| [])
                          ]
                ]
        parseFBind :: Parser (NE.NonEmpty (WithLocation FBind))
        parseFBind = inCurly $ parser `sepBy1P` SpecialComma
        createRecordUpdate ::
               WithLocation AExp
            -> NE.NonEmpty (WithLocation FBind)
            -> WithLocation AExp
        createRecordUpdate aExp fbinds =
            WithLocation
                (AExpRecordUpdate aExp fbinds)
                (SourceLocation
                     (getLocationStart $ getLocation aExp)
                     (getLocationEnd $ getLocation (NE.last fbinds)))

instance Parseable FBind where
    parser = do
        var <- parser
        expect_ OperatorEq
        e <- parser
        return $ FBind var e

instance Parseable Pat where
    parser = parseInfix (liftP1 PatSimple) PatInfix

instance Parseable LPat where
    parser =
        safeChoice
            [ minus *> liftP1 LPatNegated
            , liftA2 LPatConstructor parser (some parser)
            , liftP1 LPatSimple
            ]

instance Parseable APat where
    parser =
        safeChoice
            [ liftA2
                  APatVariable
                  parser
                  (safeOptional (expect OperatorAt *> parser))
            , liftA2 APatRecord parser (inCurly $ parser `sepByP` SpecialComma)
            , liftP1 APatConstructor
            , liftP1 APatLiteral
            , APatWildcard <$ expect KeywordUnderscore
            , inParens $ do
                  f <- parser
                  safeChoice
                      [ do expect_ SpecialComma
                           (s NE.:| rest) <- parser `sepBy1P` SpecialComma
                           return $ APatTuple f s rest
                      , return $ APatParens f
                      ]
            , inBrackets (APatList <$> parser `sepBy1P` SpecialComma)
            ]

instance Parseable FPat where
    parser = do
        var <- parser
        expect_ OperatorEq
        pat <- parser
        return $ FPat var pat

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

-- | "Wrap" a parser in parenthesis
inParens :: Parser a -> Parser a
inParens = between (expect SpecialLParen) (expect SpecialRParen)

-- | "Wrap" a parser in curly brackets
inCurly :: Parser a -> Parser a
inCurly = between (expect SpecialLCurly) (expect SpecialRCurly)

-- | "Wrap" a parser in square brackets
inBrackets :: Parser a -> Parser a
inBrackets = between (expect SpecialLBracket) (expect SpecialRBracket)

-- | "Wrap" a parser in backticks
inBackticks :: Parser a -> Parser a
inBackticks = between (expect SpecialBacktick) (expect SpecialBacktick)

sepByP :: (Parseable b, Eq b, Show b) => Parser a -> b -> Parser [a]
sepByP item sep = try item `sepEndBy` try (expect sep)

sepBy1P ::
       (Parseable b, Eq b, Show b) => Parser a -> b -> Parser (NE.NonEmpty a)
sepBy1P item sep = try item `sepEndBy1` try (expect sep)

-- | Run parser and pass result as an argument to a function
liftP1 :: (Parseable a) => (a -> b) -> Parser b
liftP1 f = f <$> parser

-- | Run 2 parsers and pass results as arguments to a function
liftP2 :: (Parseable a, Parseable b) => (a -> b -> c) -> Parser c
liftP2 f = liftA2 f parser parser

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

-- | Expect an object and ignore result
expect_ :: (Parseable a, Eq a, Show a) => a -> Parser ()
expect_ x = void $ expect x

-- | Run parser and save locations of parsed tokens
parseWithLocation :: Parser a -> Parser (WithLocation a)
parseWithLocation parser' = do
    nextTokenLocation <- getNextTokenLocation -- Get location of the next token
    let (SourceLocation start _) = fromMaybe dummyLocation nextTokenLocation
    -- Run parser
    value <- parser'
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

-- | Parse type context
parseContext :: (Parseable a) => Parser [a]
parseContext =
    fromMaybe [] <$>
    safeOptional
        (safeChoice [return <$> parser, inParens $ parser `sepByP` SpecialComma] <*
         expect OperatorBoldRArrow)

-- | Parse infix expression. All operators as treated as left-associative
--   with the same priority
parseInfix ::
       (Parseable b)
    => Parser a
    -> (WithLocation a -> WithLocation b -> WithLocation a -> a)
    -> Parser a
parseInfix parseSingle makeInfix = do
    left <- parseSingle'
    operands <- many (try parseOperand)
    return . getValue $ foldl createInfix left operands
  where
    parseSingle' = parseWithLocation parseSingle
    parseOperand = liftA2 (\op right -> (op, right)) parser parseSingle'
    createInfix left (op, right) =
        WithLocation
            (makeInfix left op right)
            (SourceLocation
                 (getLocationStart $ getLocation left)
                 (getLocationEnd $ getLocation right))
