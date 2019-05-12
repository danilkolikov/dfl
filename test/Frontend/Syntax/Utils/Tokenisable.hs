{- |
Module      :  Frontend.Syntax.Utils.Tokenisable
Description :  Conversion of AST to tokens
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module converts abstract syntax trees to tokens.
-}
module Frontend.Syntax.Utils.Tokenisable where

import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE (toList)

import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Syntax.Token

-- | Class for types which can be converted to tokens.
class Tokenisable a where
    toTokens :: a -> [Token] -- ^ Convert object to a list of tokens

instance Tokenisable Token where
    toTokens = return

instance (Tokenisable a) => Tokenisable (Maybe a) where
    toTokens Nothing = []
    toTokens (Just x) = toTokens x

instance (Tokenisable a, Tokenisable b) => Tokenisable (Either a b) where
    toTokens (Left x) = toTokens x
    toTokens (Right y) = toTokens y

instance Tokenisable IntT where
    toTokens = tokenContainsToTokens

instance Tokenisable FloatT where
    toTokens = tokenContainsToTokens

instance Tokenisable CharT where
    toTokens = tokenContainsToTokens

instance Tokenisable StringT where
    toTokens = tokenContainsToTokens

instance Tokenisable Keyword where
    toTokens = tokenContainsToTokens

instance Tokenisable Operator where
    toTokens = tokenContainsToTokens

instance Tokenisable Special where
    toTokens = tokenContainsToTokens

instance Tokenisable EOF where
    toTokens = tokenContainsToTokens

instance Tokenisable ConId where
    toTokens name = [TokenName [] (NameConId name)]

instance Tokenisable ConSym where
    toTokens name = [TokenName [] (NameConSym name)]

instance Tokenisable VarId where
    toTokens name = [TokenName [] (NameVarId name)]

instance Tokenisable VarSym where
    toTokens name = [TokenName [] (NameVarSym name)]

-- Hacky instance that supports only 4 types
instance (Tokenisable a) => Tokenisable (Qualified a) where
    toTokens (Qualified path x) =
        case toTokens x of
            [TokenName [] name] -> [TokenName path name]
            _ -> undefined

instance (Tokenisable a) => Tokenisable (WithLocation a) where
    toTokens = toTokens . getValue

instance Tokenisable Literal where
    toTokens (LiteralInteger x) = toTokens x
    toTokens (LiteralFloat x) = toTokens x
    toTokens (LiteralChar x) = toTokens x
    toTokens (LiteralString x) = toTokens x

instance Tokenisable Module where
    toTokens (ModuleExplicit name exports body) =
        [TokenKeyword KeywordModule] ++
        toTokens name ++
        toTokens (fmap (inParens . sepByComma) exports) ++
        [TokenKeyword KeywordWhere] ++ toTokens body
    toTokens (ModuleImplicit body) = toTokens body

instance Tokenisable Body where
    toTokens (Body imps) =
        toTokens
            (inCurly $
             Tokens $
             intercalate
                 [TokenSpecial SpecialSemicolon]
                 (map toTokens imps))

instance Tokenisable ImpExpList where
    toTokens ImpExpAll = toTokens (inParens $ TokenOperator OperatorDDot)
    toTokens ImpExpNothing = toTokens (inParens (Nothing :: Maybe Token))
    toTokens (ImpExpSome lst) = toTokens (inParens $ sepByComma $ NE.toList lst)

instance Tokenisable Export where
    toTokens (ExportFunction name) = toTokens name
    toTokens (ExportDataOrClass name lst) = toTokens name ++ toTokens lst
    toTokens (ExportModule name) = TokenKeyword KeywordModule : toTokens name

instance Tokenisable ImpDecl where
    toTokens (ImpDecl qualified name as spec) =
        TokenKeyword KeywordImport :
        [TokenName [] (NameVarId (VarId "qualified")) | qualified] ++
        toTokens name ++
        (case toTokens as of
             [] -> []
             toks -> TokenName [] (NameVarId (VarId "as")) : toks) ++
        toTokens spec

instance Tokenisable ImpSpec where
    toTokens (ImpSpec hiding lst) =
        [TokenName [] (NameVarId (VarId "hiding")) | hiding] ++
        toTokens (inParens $ sepByComma lst)

instance Tokenisable Import where
    toTokens (ImportFunction name) = toTokens name
    toTokens (ImportDataOrClass name lst) = toTokens name ++ toTokens lst

instance Tokenisable Type where
    toTokens (Type args) =
        toTokens $ SepBy (TokenOperator OperatorRArrow) (NE.toList args)

instance Tokenisable BType where
    toTokens (BType args) = toTokens (NotSep $ NE.toList args)

instance Tokenisable AType where
    toTokens (ATypeConstructor name) = toTokens name
    toTokens (ATypeVar name) = toTokens name
    toTokens (ATypeTuple f s rest) =
        toTokens (inParens (sepByComma $ f : s : rest))
    toTokens (ATypeList arg) = toTokens (inBrackets arg)
    toTokens (ATypeParens arg) = toTokens (inParens arg)

instance Tokenisable GTyCon where
    toTokens (GTyConNamed name) = toTokens name
    toTokens GTyConUnit = toTokens (inParens (Tokens []))
    toTokens GTyConList = toTokens (inBrackets (Tokens []))
    toTokens GTyConFunction = toTokens (inParens (TokenOperator OperatorRArrow))
    toTokens (GTyConTuple n) =
        toTokens (inParens (Tokens $ replicate (n - 1) (TokenSpecial SpecialComma)))

instance Tokenisable GCon where
    toTokens GConUnit = toTokens (inParens (Nothing :: Maybe Token))
    toTokens GConList = toTokens (inBrackets (Nothing :: Maybe Token))
    toTokens (GConTuple n) =
        toTokens
            (inParens (Tokens $ replicate (n - 1) (TokenSpecial SpecialComma)))
    toTokens (GConNamed wl) = toTokens wl

instance (Tokenisable a, Tokenisable b) => Tokenisable (FuncLabel a b) where
    toTokens (FuncLabelId name) = toTokens name
    toTokens (FuncLabelSym sym) = toTokens $ inParens sym

instance (Tokenisable a, Tokenisable b) => Tokenisable (OpLabel a b) where
    toTokens (OpLabelSym sym) = toTokens sym
    toTokens (OpLabelId name) = toTokens $ inBackticks name

instance Tokenisable GConSym where
    toTokens GConSymColon = [TokenName [] (NameConSym $ ConSym ":")]
    toTokens (GConSymOp sym) = toTokens sym


-- Helper functions

-- | Converts parts of tokens to tokens
tokenContainsToTokens :: (TokenContains a) => a -> [Token]
tokenContainsToTokens x = [toToken x]

-- | List of tokens
newtype Tokens =
    Tokens [Token]

instance Tokenisable Tokens where
    toTokens (Tokens tokens) = tokens

-- | Object between two tokens
data Between a =
    Between Token
            Token
            a

instance (Tokenisable a) => Tokenisable (Between a) where
    toTokens (Between l r x) = l : toTokens x ++ [r]

-- | Wrap object in 2 parenthesis
inParens :: a -> Between a
inParens = Between (TokenSpecial SpecialLParen) (TokenSpecial SpecialRParen)

-- | Wrap object in 2 curly brackets
inCurly :: a -> Between a
inCurly = Between (TokenSpecial SpecialLCurly) (TokenSpecial SpecialRCurly)

-- | Wrap object in 2 square brackets
inBrackets :: a -> Between a
inBrackets =
    Between (TokenSpecial SpecialLBracket) (TokenSpecial SpecialRBracket)

-- | Wrap object in 2 backticks
inBackticks :: a -> Between a
inBackticks =
    Between (TokenSpecial SpecialBacktick) (TokenSpecial SpecialBacktick)

-- | List of objects, separated by tokens
data SepBy a =
    SepBy Token
          [a]

instance (Tokenisable a) => Tokenisable (SepBy a) where
    toTokens (SepBy token xs) = intercalate [token] (map toTokens xs)

-- | Separate objects by comma
sepByComma :: [a] -> SepBy a
sepByComma = SepBy (TokenSpecial SpecialComma)

-- | Separate objects by semicolon
sepBySemicolon :: [a] -> SepBy a
sepBySemicolon = SepBy (TokenSpecial SpecialSemicolon)

-- | Not separated list of tokens
newtype NotSep a =
    NotSep [a]

instance (Tokenisable a) => Tokenisable (NotSep a) where
    toTokens (NotSep xs) = concatMap toTokens xs
