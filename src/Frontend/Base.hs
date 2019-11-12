{- |
Module      :  Frontend.Base
Description :  Base definitions for frontend steps
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base definitions for frontend processing
-}
module Frontend.Base where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Checking.Base (ImportedGroups(..))
import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Desugaring.Processor
import Frontend.Inference.Processor
import Frontend.Syntax.Processor

-- | State of the frontend processing
data FrontendState = FrontendState
    { getFrontendStateDesugaring :: ImportedGroups
    , getFrontendStateFixity :: InfixOperators
    , getFrontendStateInference :: InferenceProcessorOutput
    } deriving (Eq, Show)

-- | An empty state
emptyFrontendState :: FrontendState
emptyFrontendState =
    FrontendState
        { getFrontendStateDesugaring = emptyImportedGroups
        , getFrontendStateFixity = HM.empty
        , getFrontendStateInference = emptyInferenceProcessorOutput
        }

-- | A type of errors which can be encountered during processing source files
data FrontendProcessorError
    = FrontendProcessorErrorSyntax SyntaxProcessorError
    | FrontendProcessorErrorDesugaring DesugaringError
    | FrontendProcessorErrorInference InferenceProcessorError
    deriving (Eq, Show)

-- | An output of processing sources files
data FrontendProcessorOutput = FrontendProcessorOutput
    { getFrontendProcessorOutputDesugaredExpressions :: F.Module F.Exp -- ^ Result of desugaring
    , getFrontendProcessorOutputExpressions :: TranslatedExpressions -- ^ Translated expressions
    , getFrontendProcessorOutputState :: FrontendState -- ^ State of the processor
    } deriving (Eq, Show)

-- | An empty output
emptyFrontendProcessorOutput :: FrontendProcessorOutput
emptyFrontendProcessorOutput =
    FrontendProcessorOutput
        { getFrontendProcessorOutputDesugaredExpressions =
              error "Desugared expressions are not defined"
        , getFrontendProcessorOutputExpressions = HM.empty
        , getFrontendProcessorOutputState = emptyFrontendState
        }

-- | A debug output of processing source files
data FrontendProcessorDebugOutput = FrontendProcessorDebugOutput
    { getFrontendProcessorDebugOutputSyntax :: Maybe SyntaxProcessorDebugOutput
    , getFrontendProcessorDebugOutputDesugaring :: Maybe DesugaringDebugOutput
    , getFrontendProcessorDebugOutputInference :: Maybe InferenceProcessorDebugOutput
    }

instance Semigroup FrontendProcessorDebugOutput where
    FrontendProcessorDebugOutput s1 d1 i1 <> FrontendProcessorDebugOutput s2 d2 i2 =
        FrontendProcessorDebugOutput (s1 <|> s2) (d1 <|> d2) (i1 <|> i2)

instance Monoid FrontendProcessorDebugOutput where
    mempty = FrontendProcessorDebugOutput Nothing Nothing Nothing
