{- |
Module      :  Frontend.Syntax.Utils.Tokenisable
Description :  Conversion of AST to tokens
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module converts abstract syntax trees to tokens.
-}
module Frontend.Syntax.Utils.Tokenisable where

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

instance Tokenisable GCon where
    toTokens GConUnit = toTokens (inParens (Nothing :: Maybe Token))
    toTokens GConList = toTokens (inBrackets (Nothing :: Maybe Token))
    toTokens (GConTuple n) =
        toTokens
            (inParens (Tokens $ replicate (n - 1) (TokenSpecial SpecialComma)))
    toTokens (GConNamed wl) = toTokens wl

instance Tokenisable GTyCon where
    toTokens (GTyConNamed name) = toTokens name
    toTokens GTyConUnit = toTokens (inParens (Nothing :: Maybe Token))
    toTokens GTyConList = toTokens (inBrackets (Nothing :: Maybe Token))
    toTokens GTyConFunction = toTokens (inParens (TokenOperator OperatorRArrow))
    toTokens (GTyConTuple n) =
        toTokens (inParens (Tokens $ replicate (n - 1) (TokenSpecial SpecialComma)))

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

-- | Wrap object in 2 square brackets
inBrackets :: a -> Between a
inBrackets =
    Between (TokenSpecial SpecialLBracket) (TokenSpecial SpecialRBracket)

-- | Wrap object in 2 backticks
inBackticks :: a -> Between a
inBackticks =
    Between (TokenSpecial SpecialBacktick) (TokenSpecial SpecialBacktick)
