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

-- | Helper function for converting of parts of tokens to tokens
tokenContainsToTokens :: (TokenContains a) => a -> [Token]
tokenContainsToTokens x = [toToken x]

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
