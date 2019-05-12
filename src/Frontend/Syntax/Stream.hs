{- |
Module      :  Frontend.Syntax.Stream
Description :  Stream of tokens
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Stream of tokens for parsing.
-}
{-# LANGUAGE TypeFamilies #-}

module Frontend.Syntax.Stream
    ( TokenStream(..)
    ) where

import Text.Megaparsec (Stream(..))

import Frontend.Syntax.Position (WithLocation(..))
import qualified Frontend.Syntax.Token as T (Token(..))

-- | Stream of tokens, returned by a lexer
newtype TokenStream = TokenStream
    { getTokens :: [WithLocation T.Token]
    } deriving (Show, Eq, Ord)

instance Stream TokenStream where
    type Token TokenStream = WithLocation T.Token
    type Tokens TokenStream = TokenStream
    tokenToChunk _ = TokenStream . pure
    tokensToChunk _ = TokenStream
    chunkToTokens _ = getTokens
    chunkLength _ = length . getTokens
    chunkEmpty _ = null . getTokens
    take1_ (TokenStream []) = Nothing
    take1_ (TokenStream (t:rest)) = Just (t, TokenStream rest)
    takeN_ n s@(TokenStream tokens)
        | n <= 0 = Just (TokenStream [], s)
        | null tokens = Nothing
        | otherwise =
            let (st, en) = splitAt n tokens
             in Just (TokenStream st, TokenStream en)
    takeWhile_ f (TokenStream tokens) =
        let (st, en) = span f tokens
         in (TokenStream st, TokenStream en)
    showTokens _ = show
    -- It's undefined, because we don't display errors using Megaparsec
    reachOffset = undefined
