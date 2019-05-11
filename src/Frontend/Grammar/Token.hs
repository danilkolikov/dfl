{- |
Module      :  Frontend.Grammar.Token
Description :  Tokens of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Definition of tokens of DFL with supporting code.
-}
module Frontend.Grammar.Token where

import qualified Data.HashMap.Lazy as HM

-- | Underlying type of tokens
type TokenT = String

-- | Type representing integer numbers
newtype IntT =
    IntT Int
    deriving (Show, Eq, Ord)

-- | Type representing floating point numbers
newtype FloatT =
    FloatT Double
    deriving (Show, Eq, Ord)

-- | Type representing characters
newtype CharT =
    CharT Char
    deriving (Show, Eq, Ord)

-- | Type representing strings
newtype StringT =
    StringT String
    deriving (Show, Eq, Ord)

-- | Type representing IDs of functions
newtype VarId =
    VarId TokenT
    deriving (Show, Eq, Ord)

-- | Type representing IDs of constructors
newtype ConId =
    ConId TokenT
    deriving (Show, Eq, Ord)

-- | Type representing user-defined operators
newtype VarSym =
    VarSym TokenT
    deriving (Show, Eq, Ord)

-- | Type representing constructor operators
newtype ConSym =
    ConSym TokenT
    deriving (Show, Eq, Ord)

-- | Type representing IDs of modules
type ModId = ConId

-- | Token of DFL
data Token
    = TokenOperator Operator -- ^ Reserved operator
    | TokenKeyword Keyword -- ^ Reserved keyword
    | TokenSpecial Special -- ^ Special symbol
    | TokenInteger IntT -- ^ Integer number
    | TokenFloat FloatT -- ^ Float number
    | TokenChar CharT -- ^ Character
    | TokenString StringT -- ^ String
    | TokenName [ModId]
                Name -- ^ Possibly qualified name
    | TokenEOF EOF -- ^ End of a file
    deriving (Eq, Show, Ord)

-- | Type representing names in DFL
data Name
    = NameVarId VarId -- ^ Name of a function
    | NameConId ConId -- ^ Name of a constructor
    | NameVarSym VarSym -- ^ User-defined symbol
    | NameConSym ConSym -- ^ Constructor operator
    deriving (Eq, Show, Ord)

-- | Reserved operators
data Operator
    = OperatorDDot -- ^ Double dot: ".."
    | OperatorQDot -- ^ Quadruple dot: "::"
    | OperatorColon -- ^ Colon: ":"
    | OperatorBackslash -- ^ Backslash: "\\"
    | OperatorBar -- ^ Vertical bar: "|"
    | OperatorLArrow -- ^ Left arrow: "<-"
    | OperatorRArrow -- ^ Right arrow: "->"
    | OperatorAt -- ^ At: "@"
    | OperatorBoldRArrow -- ^ Bold right arrow: "=>"
    | OperatorEq -- ^ Equality sign: "="
    deriving (Eq, Show, Ord)

-- | Reserved keywords
data Keyword
    = KeywordCase -- ^ "case"
    | KeywordClass -- ^ "class"
    | KeywordData -- ^ "data"
    | KeywordDeriving -- ^ "deriving"
    | KeywordDo -- ^ "do"
    | KeywordElse -- ^ "else"
    | KeywordIf -- ^ "if"
    | KeywordImport -- ^ "import"
    | KeywordInfix -- ^ "infix"
    | KeywordInfixR -- ^ "infixr"
    | KeywordInfixL -- ^ "infixl"
    | KeywordIn -- ^ "in"
    | KeywordInstance -- ^ "instance"
    | KeywordLet -- ^ "let"
    | KeywordModule -- ^ "module"
    | KeywordNewType -- ^ "newtype"
    | KeywordOf -- ^ "of"
    | KeywordThen -- ^ "then"
    | KeywordType -- ^ "type"
    | KeywordWhere -- ^ "where"
    | KeywordUnderscore -- ^ "_"
    deriving (Eq, Show, Ord)

-- | Special symbols
data Special
    = SpecialLParen -- ^ Opening parenthesis: "("
    | SpecialRParen -- ^ Closing parenthesis: ")"
    | SpecialComma -- ^ Comma: ","
    | SpecialSemicolon -- ^ Semicolon: ";"
    | SpecialLBracket -- ^ Opening square bracket: "["
    | SpecialRBracket -- ^ Closing square bracket: "]"
    | SpecialBacktick -- ^ Backtick: "`"
    | SpecialLCurly -- ^ Opening curly bracket: "{"
    | SpecialRCurly -- ^ Closing curly bracket: "}"
    deriving (Eq, Show, Ord)

-- | Type representing end of file
data EOF =
    EOF
    deriving (Eq, Show, Ord)

-- | Mapping between strings and keywords
keywords :: HM.HashMap String Keyword
keywords =
    HM.fromList
        [ ("case", KeywordCase)
        , ("class", KeywordClass)
        , ("data", KeywordData)
        , ("deriving", KeywordDeriving)
        , ("do", KeywordDo)
        , ("else", KeywordElse)
        , ("if", KeywordIf)
        , ("import", KeywordImport)
        , ("infixr", KeywordInfixR)
        , ("infixl", KeywordInfixL)
        , ("infix", KeywordInfix)
        , ("instance", KeywordInstance)
        , ("in", KeywordIn)
        , ("let", KeywordLet)
        , ("module", KeywordModule)
        , ("newtype", KeywordNewType)
        , ("of", KeywordOf)
        , ("then", KeywordThen)
        , ("type", KeywordType)
        , ("where", KeywordWhere)
        , ("_", KeywordUnderscore)
        ]

-- | Mapping between strings and operators
operators :: HM.HashMap String Operator
operators =
    HM.fromList
        [ ("..", OperatorDDot)
        , ("::", OperatorQDot)
        , (":", OperatorColon)
        , ("\\", OperatorBackslash)
        , ("|", OperatorBar)
        , ("<-", OperatorLArrow)
        , ("->", OperatorRArrow)
        , ("@", OperatorAt)
        , ("=>", OperatorBoldRArrow)
        , ("=", OperatorEq)
        ]

-- | Mapping between characters and special symbols
specialSymbols :: HM.HashMap Char Special
specialSymbols =
    HM.fromList
        [ ('(', SpecialLParen)
        , (')', SpecialRParen)
        , (',', SpecialComma)
        , (';', SpecialSemicolon)
        , ('[', SpecialLBracket)
        , (']', SpecialRBracket)
        , ('`', SpecialBacktick)
        , ('{', SpecialLCurly)
        , ('}', SpecialRCurly)
        ]

-- | Class of types, which are contained inside the type 'Token'
class TokenContains a where
    getFromToken :: Token -> Maybe a
    toToken :: a -> Token

-- | Class of types, which are contained inside the type 'Name'
class NameContains a where
    getFromName :: Name -> Maybe a

instance TokenContains Special where
    getFromToken (TokenSpecial s) = Just s
    getFromToken _ = Nothing
    toToken = TokenSpecial

instance TokenContains Keyword where
    getFromToken (TokenKeyword s) = Just s
    getFromToken _ = Nothing
    toToken = TokenKeyword

instance TokenContains Operator where
    getFromToken (TokenOperator s) = Just s
    getFromToken _ = Nothing
    toToken = TokenOperator

instance TokenContains IntT where
    getFromToken (TokenInteger x) = Just x
    getFromToken _ = Nothing
    toToken = TokenInteger

instance TokenContains FloatT where
    getFromToken (TokenFloat x) = Just x
    getFromToken _ = Nothing
    toToken = TokenFloat

instance TokenContains CharT where
    getFromToken (TokenChar x) = Just x
    getFromToken _ = Nothing
    toToken = TokenChar

instance TokenContains StringT where
    getFromToken (TokenString x) = Just x
    getFromToken _ = Nothing
    toToken = TokenString

-- | Structure which contains a possibly qualified name
data NameWithPath =
    NameWithPath [ModId]
                 Name

instance TokenContains NameWithPath where
    getFromToken (TokenName path name) = Just $ NameWithPath path name
    getFromToken _ = Nothing
    toToken (NameWithPath path name) = TokenName path name

instance TokenContains EOF where
    getFromToken (TokenEOF e) = Just e
    getFromToken _ = Nothing
    toToken = TokenEOF

instance NameContains VarId where
    getFromName (NameVarId n) = Just n
    getFromName _ = Nothing

instance NameContains ConId where
    getFromName (NameConId n) = Just n
    getFromName _ = Nothing

instance NameContains VarSym where
    getFromName (NameVarSym n) = Just n
    getFromName _ = Nothing

instance NameContains ConSym where
    getFromName (NameConSym n) = Just n
    getFromName _ = Nothing
