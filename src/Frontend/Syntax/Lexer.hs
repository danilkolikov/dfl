{- |
Module      :  Frontend.Syntax.Lexer
Description :  Lexer of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Lexer of DFL. Definitions of tokens follow ones from
<https://www.haskell.org/onlinereport/haskell2010/haskellch10.html Haskell 2010>.
-}
module Frontend.Syntax.Lexer
    ( Lexer
    , Lexable(..)
    , sourceLexer
    , programLexer
    , whitespace
    ) where

import Control.Applicative ((<$>), empty, liftA2)
import Control.Monad.Combinators (between, choice, many, sepBy, skipMany)
import Data.Char (isDigit, isLower, isSpace, isUpper)
import Data.Functor ((<$))
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import qualified Data.Set as S

import Text.Megaparsec
    ( Parsec
    , (<?>)
    , anySingle
    , customFailure
    , eof
    , satisfy
    , takeWhile1P
    , takeWhileP
    , token
    , try
    )
import Text.Megaparsec.Char
    ( char
    , char'
    , hexDigitChar
    , octDigitChar
    , space1
    , spaceChar
    )
import qualified Text.Megaparsec.Char.Lexer as L

import Frontend.Syntax.Layout (LayoutError, restoreMissingTokens)
import Frontend.Syntax.Position
    ( SourceLocation(..)
    , WithLocation(..)
    , getSourcePosition
    , getValue
    )
import Frontend.Syntax.Token

-- | Type of a lexer.
--   Lexer is a parser which consumes strings and produces array of tokens
type Lexer = Parsec LayoutError String

-- | Class for types which can be "parsed" from a string
class Lexable a where
    lexer :: Lexer a -- ^ Lexer for this type

instance Lexable Special where
    lexer = token (`HM.lookup` specialSymbols) S.empty

isSymbol :: Char -> Bool
isSymbol = (`elem` "!#$%&*+./<=>@\\^|-~:")

operatorLexer :: Lexer String
operatorLexer = takeWhile1P (Just "symbol") isSymbol

prefixedOperatorLexer :: Lexer Char -> Lexer String
prefixedOperatorLexer first =
    liftA2 (:) first (takeWhileP (Just "symbol") isSymbol)

isSmall :: Char -> Bool
isSmall c = isLower c || c == '_'

isLarge :: Char -> Bool
isLarge = isUpper

isNameCharacter :: Char -> Bool
isNameCharacter c = isSmall c || isLarge c || c == '\''

nameLexer :: Lexer Char -> Lexer String
nameLexer first =
    liftA2 (:) first (takeWhileP (Just "letter, _ or \'") isNameCharacter)

lowerCaseNameLexer :: Lexer String
lowerCaseNameLexer = nameLexer (satisfy isSmall)

upperCaseNameLexer :: Lexer String
upperCaseNameLexer = nameLexer (satisfy isLarge)

lookupOrFail :: (Eq a, Hashable a) => HM.HashMap a b -> a -> Lexer b
lookupOrFail list x =
    case HM.lookup x list of
        Just res -> return res
        Nothing -> empty

lookupAndFail :: (Eq a, Hashable a) => HM.HashMap a b -> a -> Lexer a
lookupAndFail list x =
    case HM.lookup x list of
        Just _ -> empty
        Nothing -> return x

instance Lexable Operator where
    lexer = operatorLexer >>= lookupOrFail operators

instance Lexable Keyword where
    lexer = lowerCaseNameLexer >>= lookupOrFail keywords

instance Lexable IntT where
    lexer =
        IntT <$>
        safeChoice
            [ char '0' *> char' 'o' *> L.octal
            , char '0' *> char' 'x' *> L.hexadecimal
            , L.decimal
            ]

instance Lexable FloatT where
    lexer = FloatT <$> L.float

escapedCharLexer :: Lexer Char
escapedCharLexer =
    try (do _ <- char '\\'
            next <- anySingle
            case next of
                'a' -> return '\a'
                'b' -> return '\b'
                'f' -> return '\f'
                'n' -> return '\n'
                'r' -> return '\r'
                't' -> return '\t'
                'v' -> return '\v'
                '\\' -> return '\\'
                '"' -> return '"'
                '\'' -> return '\''
                c
                    | isDigit c -> return c
                    | c == 'o' -> octDigitChar
                    | c == 'x' -> hexDigitChar
                    | otherwise -> empty) <?>
    "escaped character"

graphicLexer :: Char -> Lexer Char
graphicLexer extra =
    satisfy
        (\c ->
             isSmall c ||
             isUpper c ||
             isDigit c ||
             isSpace c || HM.member c specialSymbols || (c == extra)) <?>
    "graphic character"

stringCharLexer :: Char -> Lexer Char
stringCharLexer c =
    safeChoice [graphicLexer c, escapedCharLexer] <?> "string character"

instance Lexable CharT where
    lexer = CharT <$> between (char '\'') (char '\'') (stringCharLexer '"')

instance Lexable StringT where
    lexer =
        StringT <$>
        between (char '"') (char '"') (concat <$> (parseLine `sepBy` parseGap))
      where
        parseLine :: Lexer String
        parseLine = many $ stringCharLexer '\''
        parseGap :: Lexer ()
        parseGap = between (char '\\') (char '\\') (skipMany spaceChar)

instance Lexable VarId where
    lexer = VarId <$> (lowerCaseNameLexer >>= lookupAndFail keywords)

instance Lexable ConId where
    lexer = ConId <$> upperCaseNameLexer

moduleLexer :: Lexer [ModId]
moduleLexer = many (try $ lexer <* char '.')

instance Lexable VarSym where
    lexer =
        VarSym <$>
        (prefixedOperatorLexer (satisfy (\c -> isSymbol c && c /= ':')) >>=
         lookupAndFail operators)

instance Lexable ConSym where
    lexer =
        ConSym <$>
        (prefixedOperatorLexer (char ':') >>= lookupAndFail operators)

instance Lexable Name where
    lexer =
        safeChoice
            [ NameVarId <$> lexer
            , NameConId <$> lexer
            , NameVarSym <$> lexer
            , NameConSym <$> lexer
            ]

instance Lexable Token where
    lexer =
        safeChoice
            [ TokenOperator <$> lexer
            , TokenKeyword <$> lexer
            , TokenSpecial <$> lexer
            , TokenFloat <$> lexer
            , TokenInteger <$> lexer
            , TokenChar <$> lexer
            , TokenString <$> lexer
            , liftA2 TokenName moduleLexer lexer
            , TokenEOF EOF <$ eof
            ]

instance (Lexable a) => Lexable (WithLocation a) where
    lexer = do
        startPos <- getSourcePosition
        res <- lexer
        WithLocation res . SourceLocation startPos <$> getSourcePosition

-- | Lexer for whitespace and comments
whitespace :: Lexer ()
whitespace = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "-- "
    blockComment = L.skipBlockCommentNested "{-" "-}"

-- | Lexer for a program. It reads token while EOF is not reached
programLexer :: Lexer [WithLocation Token]
programLexer = many (try readSingle)
  where
    readSingle :: Lexer (WithLocation Token)
    readSingle = do
        whitespace
        token' <- lexer
        if getValue token' == TokenEOF EOF
            then empty
            else return token'

-- | Lexer of source files. It inserts missing tokens, based on code layout
sourceLexer :: Lexer [WithLocation Token]
sourceLexer = do
    program <- programLexer
    whitespace
    eof' <- lexer
    case restoreMissingTokens program of
        Left lexerError -> customFailure lexerError
        Right result -> return $ result ++ [eof']

-- | Safely select the first successful lexer
safeChoice :: [Lexer a] -> Lexer a
safeChoice = choice . map try
