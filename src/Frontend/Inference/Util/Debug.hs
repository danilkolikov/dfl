{- |
Module      :  Frontend.Inference.Util.Debug
Description :  Functions for debug output
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions and types for easier debug output
-}
module Frontend.Inference.Util.Debug
    ( WithDebugOutput
    , runWithDebugOutput
    , writeDebugOutput
    , wrapEither
    , wrapErrorAndDebugOutput
    , wrapError
    , wrapDebugOutput
    , wrapResult
    , mapDebugOutput
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Control.Monad.Trans.Writer.Lazy (Writer, runWriter, tell)
import Data.Bifunctor (first)

-- | A type of functions which support raising exceptions and collecting
-- | debug information
type WithDebugOutput e d = ExceptT e (Writer d)

-- | Executes the function and collects debug output
runWithDebugOutput :: (Monoid d) => WithDebugOutput e d a -> (Either e a, d)
runWithDebugOutput = runWriter . runExceptT

-- | Writes debug output
writeDebugOutput :: (Monoid d) => d -> WithDebugOutput e d ()
writeDebugOutput = lift . tell

-- | Wraps an Either and maps an error
wrapEither :: (Monoid d) => (l -> e) -> Either l a -> WithDebugOutput e d a
wrapEither wrapper = except . first wrapper

-- | Wraps a result and maps error and debug output
wrapErrorAndDebugOutput ::
       (Monoid d)
    => (l -> e)
    -> (c -> d)
    -> (Either l a, c)
    -> WithDebugOutput e d a
wrapErrorAndDebugOutput errorWrapper debugWrapper (result, debugOutput) =
    writeDebugOutput (debugWrapper debugOutput) >>
    wrapEither errorWrapper result

-- | Wraps a result and maps error
wrapError :: (Monoid d) => (l -> e) -> (Either l a, d) -> WithDebugOutput e d a
wrapError wrapper = wrapErrorAndDebugOutput wrapper id

-- | Wraps a result and maps debug output
wrapDebugOutput ::
       (Monoid d) => (c -> d) -> (Either e a, c) -> WithDebugOutput e d a
wrapDebugOutput = wrapErrorAndDebugOutput id

-- | Wraps a result
wrapResult :: (Monoid d) => (Either e a, d) -> WithDebugOutput e d a
wrapResult = wrapErrorAndDebugOutput id id

-- | Maps type of a debug output
mapDebugOutput ::
       (Monoid c, Monoid d)
    => (c -> d)
    -> WithDebugOutput e c a
    -> WithDebugOutput e d a
mapDebugOutput wrapper = wrapDebugOutput wrapper . runWithDebugOutput
