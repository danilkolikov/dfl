{- |
Module      :  Frontend.Processor
Description :  Functions for processing of a single source file
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for processing of a single source file - syntax analysis, desugaring, etc
-}
module Frontend.Processor where

import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Except as E (Except, except, runExcept)
import qualified Control.Monad.Trans.State as ST
    ( StateT
    , gets
    , modify
    , runStateT
    )
import Data.Bifunctor (first)

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Desugaring.Processor
import Frontend.Syntax.Analyser

-- | Errors which can be encountered during processing of a file
data SourceProcessingError
    = SourceProcessingErrorSyntax SyntaxError
    | SourceProcessingErrorDesugaring DesugaringError

-- | State of processing of a single file
data SourceProcessingState = SourceProcessingState
    { getSyntaxAnalyserState :: AnalyserState
    , getDesugaringState :: DesugaringState
    }

-- | Type of the file processor
type SourceProcessor a
     = ST.StateT SourceProcessingState (E.Except SourceProcessingError) a

-- | Process single file
runSourceProcessor ::
       String
    -> SourceProcessingState
    -> Either SourceProcessingError (F.Module, SourceProcessingState)
runSourceProcessor input st =
    E.runExcept (ST.runStateT (sourceProcessor input) st)

-- | Process source file
sourceProcessor :: String -> SourceProcessor F.Module
sourceProcessor input = do
    analyserState <- ST.gets getSyntaxAnalyserState -- Parse source file
    let syntaxResult = wrapSyntaxError $ runSyntaxAnalyser input analyserState
    (parsedModule, newAnalyserState) <- except syntaxResult
    ST.modify $ \s -> s {getSyntaxAnalyserState = newAnalyserState}
    -- Desugar AST in the file
    desugaringState <- ST.gets getDesugaringState
    let desugaringResult =
            wrapDesugaringError $
            desugarParsedModule parsedModule desugaringState
    (desugaredModule, newDesugaringState) <- except desugaringResult
    ST.modify $ \s -> s {getDesugaringState = newDesugaringState}
    return desugaredModule
  where
    except = lift . E.except

-- | Wrap error which happened during syntax analysis
wrapSyntaxError :: Either SyntaxError a -> Either SourceProcessingError a
wrapSyntaxError = first SourceProcessingErrorSyntax

-- | Wrap error which happened during desugaring
wrapDesugaringError ::
       Either DesugaringError a -> Either SourceProcessingError a
wrapDesugaringError = first SourceProcessingErrorDesugaring
