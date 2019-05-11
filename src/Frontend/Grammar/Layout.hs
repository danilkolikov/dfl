{- |
Module      :  Frontend.Grammar.Layout
Description :  Functions for layout-based parsing
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with functions for restoring missing tokens, using information about
layout of source files.

Functions from this module implement algorithms defined in the specification of
<https://www.haskell.org/onlinereport/haskell2010/haskellch10.html Haskell 2010>.
-}
module Frontend.Grammar.Layout
    ( Layout(..)
    , LayoutError(..)
    , restoreMissingTokens
    , withIndents
    , withExpectedIndent
    , getFirstIndent
    , filterConsequentIndents
    , prepareLayout
    , insertIndents
    , algorithmL
    ) where

import Control.Applicative (liftA2)
import Control.Monad.Trans.State (State, evalState, get, put)
import Data.Maybe (maybeToList)

import Text.Megaparsec (ShowErrorComponent(..))

import Frontend.Grammar.Position
    ( SourceLocation(..)
    , SourcePosition(..)
    , WithLocation(..)
    , dummyLocation
    )
import Frontend.Grammar.Token (Keyword(..), Special(..), Token(..))

-- | Type describes information about layout of source files
data Layout
    = LayoutToken (WithLocation Token) -- ^ Token
    | LayoutIndent Int -- ^ Indent of the current line
    | LayoutExpectedIndent Int -- ^ Expected indent of following tokens
    deriving (Eq, Show)

-- | Structure for indentation tracking
type IndentTracker a = State Int a

-- | Function adds expected indents, if they're required
withExpectedIndent ::
       WithLocation Token -- ^ Current token
    -> Maybe (WithLocation Token) -- ^ Following token (if exists)
    -> [Layout]
withExpectedIndent tok next'
    -- We expect { after these keywords
    | (WithLocation (TokenKeyword keyword) _) <- tok
    , keyword `elem` [KeywordLet, KeywordWhere, KeywordDo, KeywordOf] =
        case next' of
            Nothing
              -- No more tokens, expect 0 indent
             -> [LayoutToken tok, LayoutExpectedIndent 0]
            Just (WithLocation t (SourceLocation (SourcePosition _ column) _))
                | (TokenSpecial SpecialLCurly) <- t
                  -- { found, just return token
                 -> [LayoutToken tok]
                | otherwise
                  -- Missing {, expect following tokens to be indented
                  -- as the next one
                 -> [LayoutToken tok, LayoutExpectedIndent column]
    -- We don't expect indent after other tokens
    | otherwise = [LayoutToken tok]

-- | Function adds all indents, surrounding a token
withIndents ::
       WithLocation Token -- ^ Current token
    -> Maybe (WithLocation Token) -- ^ Following token (if exists)
    -> IndentTracker [Layout]
withIndents token' next =
    let withExpected = withExpectedIndent token' next
        SourcePosition curLine curColumn =
            getLocationStart . getLocation $ token'
     in do line <- get
           if line < curLine
              -- This token is the first on the line, we should save its indent
               then do
                   put curLine
                   return $ LayoutIndent curColumn : withExpected
               else return withExpected

-- | Function returns expected indent of the first token from the provided list
getFirstIndent :: [WithLocation Token] -> Maybe Layout
getFirstIndent [] = Nothing
getFirstIndent (WithLocation token' location:_)
    -- If the first token is { or module, don't add expected indent
    | token' == TokenSpecial SpecialLCurly ||
          token' == TokenKeyword KeywordModule = Nothing
    -- Otherwise, expect all following tokens to be indented as the first token
    | otherwise =
        let firstColumn = getSourceColumn . getLocationStart $ location
         in Just $ LayoutExpectedIndent firstColumn

-- | Function transforms list of 'Token'-s to 'Layout' by inserting
--   indents and expected indents
insertIndents :: [WithLocation Token] -> IndentTracker [Layout]
insertIndents [] = return []
insertIndents [token'] = withIndents token' Nothing
insertIndents (token':rest@(next:_)) =
    liftA2 (++) (withIndents token' (Just next)) (insertIndents rest)

-- | Function removes indents following expected indents
filterConsequentIndents :: [Layout] -> [Layout]
filterConsequentIndents [] = []
filterConsequentIndents ts@[_] = ts
filterConsequentIndents (first:ts@(second:rest))
    -- Remove indents following expected indents
    | LayoutExpectedIndent _ <- first
    , LayoutIndent _ <- second = first : filterConsequentIndents rest
    | otherwise = first : filterConsequentIndents ts

-- | Function prepares layout information for restoring of missing tokens
prepareLayout :: [WithLocation Token] -> [Layout]
prepareLayout tokens =
    let firstIndent = getFirstIndent tokens
        followingIndents = evalState (insertIndents tokens) 0
     in filterConsequentIndents (maybeToList firstIndent ++ followingIndents)

-- | Type represents possible errors which may happen during restoration of layout.
data LayoutError
   -- | Error caused by a redundand closing }
    = LayoutErrorRedundantClosingBracket SourceLocation
   -- | Error caused by a missing closing } in the end of file
    | LayoutErrorMissingClosingBracket
    deriving (Show, Eq, Ord)

instance ShowErrorComponent LayoutError where
    showErrorComponent e = "layout error: " ++ show e

-- | Function restores missing tokens by the provided layout information
algorithmL ::
       [Layout] -- ^ Layout of tokens
    -> [Int] -- ^ Stack of indents
    -> Either LayoutError [WithLocation Token]
algorithmL tokens stack
    | LayoutIndent n:ts <- tokens
    , m:ms <- stack =
        case compare n m of
            LT -> (closingCurly :) <$> algorithmL tokens ms
            EQ -> (semicolon :) <$> algorithmL ts stack
            GT -> algorithmL ts stack
    | LayoutIndent _:ts <- tokens
    , [] <- stack = algorithmL ts stack
    | LayoutExpectedIndent n:ts <- tokens =
        case stack of
            m:ms
                | n > m -> (openingCurly :) <$> algorithmL ts (n : m : ms)
                | otherwise ->
                    (openingCurly :) . (closingCurly :) <$>
                    algorithmL (LayoutIndent n : ts) stack
            []
                | n > 0 -> (openingCurly :) <$> algorithmL ts [n]
                | otherwise ->
                    (openingCurly :) . (closingCurly :) <$>
                    algorithmL (LayoutIndent n : ts) stack
    | LayoutToken t:ts <- tokens =
        case t of
            (WithLocation (TokenSpecial SpecialRCurly) loc) ->
                case stack of
                    0:ms -> (t :) <$> algorithmL ts ms
                    _ -> Left (LayoutErrorRedundantClosingBracket loc)
            (WithLocation (TokenSpecial SpecialLCurly) _) ->
                (t :) <$> algorithmL ts (0 : stack)
          -- Kludge for the let ... in construction
            (WithLocation (TokenKeyword KeywordIn) _) ->
                case stack of
                    m:ms
                        | m /= 0 ->
                            (closingCurly :) . (t :) <$> algorithmL ts ms
                        | otherwise -> (t :) <$> algorithmL ts stack
                    _ -> (t :) <$> algorithmL ts stack
            _ -> (t :) <$> algorithmL ts stack
    | [] <- tokens =
        case stack of
            m:ms
                | m /= 0 -> (closingCurly :) <$> algorithmL [] ms
                | otherwise -> Left LayoutErrorMissingClosingBracket
            [] -> Right []
  where
    implicitSymbol :: Special -> WithLocation Token
    implicitSymbol c = WithLocation (TokenSpecial c) dummyLocation
    semicolon :: WithLocation Token
    semicolon = implicitSymbol SpecialSemicolon
    openingCurly :: WithLocation Token
    openingCurly = implicitSymbol SpecialLCurly
    closingCurly :: WithLocation Token
    closingCurly = implicitSymbol SpecialRCurly

-- | Function restores missing tokens based on layout information
restoreMissingTokens ::
       [WithLocation Token] -> Either LayoutError [WithLocation Token]
restoreMissingTokens = (`algorithmL` []) . prepareLayout
