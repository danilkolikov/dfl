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
    , WithDebugOutputT
    , runWithDebugOutputT
    , liftInner
    , writeDebugOutput
    , raiseError
    , wrapEither
    , wrapErrorAndDebugOutput
    , wrapErrorAndDebugOutputT
    , wrapError
    , wrapDebugOutput
    , wrapResult
    , mapDebugOutput
    , lookupMapValue
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Control.Monad.Trans.Writer.Lazy
    ( Writer
    , WriterT
    , runWriter
    , runWriterT
    , tell
    )
import Data.Bifunctor (first)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)

-- | A type of functions which support raising exceptions and collecting
-- | debug information
type WithDebugOutput e d = ExceptT e (Writer d)

-- | Executes the function and collects debug output
runWithDebugOutput :: (Monoid d) => WithDebugOutput e d a -> (Either e a, d)
runWithDebugOutput = runWriter . runExceptT

-- | A type of functions which support raising exceptions and collecting
-- | debug information and supports an inner monad
type WithDebugOutputT e d m = ExceptT e (WriterT d m)

-- | Executes the function and collects debug output
runWithDebugOutputT ::
       (Monad m, Monoid d) => WithDebugOutputT e d m a -> m (Either e a, d)
runWithDebugOutputT = runWriterT . runExceptT

-- | Lifts an inner monad
liftInner :: (Monad m, Monoid d) => m a -> WithDebugOutputT e d m a
liftInner = lift . lift

-- | Writes debug output
writeDebugOutput :: (Monad m, Monoid d) => d -> WithDebugOutputT e d m ()
writeDebugOutput = lift . tell

-- | Raises an error
raiseError :: (Monad m, Monoid d) => e -> WithDebugOutputT e d m a
raiseError = except . Left

-- | Wraps an Either and maps an error
wrapEither ::
       (Monad m, Monoid d) => (l -> e) -> Either l a -> WithDebugOutputT e d m a
wrapEither wrapper = except . first wrapper

-- | Wraps a result and maps error and debug output
wrapErrorAndDebugOutput ::
       (Monad m, Monoid d)
    => (l -> e)
    -> (c -> d)
    -> (Either l a, c)
    -> WithDebugOutputT e d m a
wrapErrorAndDebugOutput errorWrapper debugWrapper (result, debugOutput) =
    writeDebugOutput (debugWrapper debugOutput) >>
    wrapEither errorWrapper result

-- | Wraps a result and maps error and debug output
wrapErrorAndDebugOutputT ::
       (Monad m, Monoid d)
    => (l -> e)
    -> (c -> d)
    -> m (Either l a, c)
    -> WithDebugOutputT e d m a
wrapErrorAndDebugOutputT errorWrapper debugWrapper monad = do
    (result, debugOutput) <- liftInner monad
    writeDebugOutput (debugWrapper debugOutput)
    wrapEither errorWrapper result

-- | Wraps a result and maps error
wrapError ::
       (Monad m, Monoid d)
    => (l -> e)
    -> (Either l a, d)
    -> WithDebugOutputT e d m a
wrapError wrapper = wrapErrorAndDebugOutput wrapper id

-- | Wraps a result and maps debug output
wrapDebugOutput ::
       (Monad m, Monoid d)
    => (c -> d)
    -> (Either e a, c)
    -> WithDebugOutputT e d m a
wrapDebugOutput = wrapErrorAndDebugOutput id

-- | Wraps a result and maps debug output
wrapDebugOutputT ::
       (Monad m, Monoid d)
    => (c -> d)
    -> m (Either e a, c)
    -> WithDebugOutputT e d m a
wrapDebugOutputT = wrapErrorAndDebugOutputT id

-- | Wraps a result
wrapResult :: (Monad m, Monoid d) => (Either e a, d) -> WithDebugOutputT e d m a
wrapResult = wrapErrorAndDebugOutput id id

-- | Maps type of a debug output
mapDebugOutput ::
       (Monad m, Monoid c, Monoid d)
    => (c -> d)
    -> WithDebugOutputT e c m a
    -> WithDebugOutputT e d m a
mapDebugOutput wrapper = wrapDebugOutputT wrapper . runWithDebugOutputT

-- | Finds a value in a map or throws an error
lookupMapValue ::
       (Eq k, Hashable k, Monoid d, Monad m)
    => e
    -> k
    -> HM.HashMap k a
    -> WithDebugOutputT e d m a
lookupMapValue e key m =
    case HM.lookup key m of
        Just result -> return result
        Nothing -> raiseError e
