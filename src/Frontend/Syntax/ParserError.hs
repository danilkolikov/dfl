{-# LANGUAGE FlexibleContexts #-}

{- |
Module      :  Frontend.Syntax.ParserError
Description :  Functions for handling parser errors
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for handling errors of Megaparsec
-}
module Frontend.Syntax.ParserError where

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Text.Megaparsec
    ( ErrorFancy(..)
    , ErrorItem(..)
    , ParseError(..)
    , ParseErrorBundle(..)
    , PosState(..)
    , Token
    )

import Frontend.Syntax.Position
    ( SourceLocation(..)
    , SourcePosition(..)
    , castSourcePosition
    )

-- | Converts a Megaparsec error into a user-defined data type, which includes
-- a source position
wrapPositionBundle ::
       (Show (Token s))
    => (SourcePosition -> Maybe (Token s) -> [String] -> a)
    -> (SourcePosition -> ErrorFancy e -> a)
    -> ParseErrorBundle s e
    -> a
wrapPositionBundle wrapParserError wrapInnerError bundle =
    let firstError = NE.head . bundleErrors $ bundle
        position =
            castSourcePosition . pstateSourcePos . bundlePosState $ bundle
     in case firstError of
            TrivialError _ got expected ->
                wrapParserError
                    position
                    (getTokens' <$> got)
                    (getLabel <$> S.toList expected)
            FancyError _ errors -> wrapInnerError position (S.findMin errors)

-- | Converts a Megaparsec error into a user-defined data type, which includes
-- a source location
wrapLocationBundle ::
       (Show (Token s))
    => (Int -> SourceLocation)
    -> (SourceLocation -> Maybe (Token s) -> [String] -> a)
    -> (SourceLocation -> ErrorFancy e -> a)
    -> ParseErrorBundle s e
    -> a
wrapLocationBundle getLocation wrapParserError wrapInnerError bundle =
    let firstError = NE.head . bundleErrors $ bundle
     in case firstError of
            TrivialError pos got expected ->
                wrapParserError
                    (getLocation pos)
                    (getTokens' <$> got)
                    (getLabel <$> S.toList expected)
            FancyError pos errors ->
                wrapInnerError (getLocation pos) (S.findMin errors)

-- Helper functions
-- | Get tokens from an ErrorItem
getTokens' :: ErrorItem a -> a
getTokens' (Tokens ts) = NE.head ts
getTokens' _ = undefined

-- | Get label from an ErrorItem
getLabel :: (Show a) => ErrorItem a -> String
getLabel (Label chars) = NE.toList chars
getLabel (Tokens ts) = concatMap show ts
getLabel EndOfInput = "End Of Input"
