{- |
Module      :  Frontend.Processor
Description :  Processor of frontend steps
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of DFL frontend
-}
module Frontend.Processor
    ( FrontendProcessorError(..)
    , FrontendProcessorOutput(..)
    , emptyFrontendProcessorOutput
    , FrontendProcessorDebugOutput(..)
    , processSourceFile
    , HeaderProcessorError(..)
    , HeaderProcessorOutput(..)
    , HeaderProcessorDebugOutput(..)
    , processModuleHeader
    ) where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Processor
import Frontend.Inference.Processor
import Frontend.Syntax.HeaderProcessor
import Frontend.Syntax.Processor
import Util.Debug

-- | A type of errors which can be encountered during processing source files
data FrontendProcessorError
    = FrontendProcessorErrorSyntax SyntaxProcessorError
    | FrontendProcessorErrorDesugaring DesugaringError
    | FrontendProcessorErrorInference InferenceProcessorError
    deriving (Eq, Show)

-- | An output of processing sources files
data FrontendProcessorOutput = FrontendProcessorOutput
    { getFrontendProcessorOutputInfix :: InfixOperators -- ^ A map of infix operators
    , getFrontendProcessorOutputDesugaring :: DesugaringState -- ^ A desugaring state
    , getFrontendProcessorOutputInference :: InferenceProcessorOutput -- ^ Output of inference
    } deriving (Eq, Show)

-- | An empty output
emptyFrontendProcessorOutput :: FrontendProcessorOutput
emptyFrontendProcessorOutput =
    FrontendProcessorOutput
        { getFrontendProcessorOutputInfix = HM.empty
        , getFrontendProcessorOutputDesugaring = emptyDesugaringState
        , getFrontendProcessorOutputInference = defaultInferenceProcessorOutput
        }

-- | A debug output of processing source files
data FrontendProcessorDebugOutput = FrontendProcessorDebugOutput
    { getFrontendProcessorDebugOutputSyntax :: Maybe SyntaxProcessorDebugOutput
    , getFrontendProcessorDebugOutputDesugaring :: Maybe DesugaringState
    , getFrontendProcessorDebugOutputInference :: Maybe InferenceProcessorDebugOutput
    }

instance Semigroup FrontendProcessorDebugOutput where
    FrontendProcessorDebugOutput s1 d1 i1 <> FrontendProcessorDebugOutput s2 d2 i2 =
        FrontendProcessorDebugOutput (s1 <|> s2) (d1 <|> d2) (i1 <|> i2)

instance Monoid FrontendProcessorDebugOutput where
    mempty = FrontendProcessorDebugOutput Nothing Nothing Nothing

-- | Processes a source file
processSourceFile ::
       FrontendProcessorOutput
    -> String
    -> TokenStream
    -> ( Either FrontendProcessorError FrontendProcessorOutput
       , FrontendProcessorDebugOutput)
processSourceFile initialState fileName stream
    | FrontendProcessorOutput { getFrontendProcessorOutputInfix = initialInfixOperators
                              , getFrontendProcessorOutputDesugaring = initialDesugaringState
                              , getFrontendProcessorOutputInference = initialInferenceState
                              } <- initialState =
        runWithDebugOutput $ do
            SyntaxProcessorOutput { getSyntaxProcessorOutputInfix = infixOperators
                                  , getSyntaxProcessorOutputAst = ast
                                  } <-
                wrapErrorAndDebugOutput
                    FrontendProcessorErrorSyntax
                    (\debug ->
                         mempty
                             { getFrontendProcessorDebugOutputSyntax =
                                   Just debug
                             }) $
                processModuleSyntax initialInfixOperators fileName stream
            DesugaringOutput { getDesugaringOutputAst = desugaredAst
                             , getDesugaringOutputState = desugaringState
                             } <-
                wrapEither FrontendProcessorErrorDesugaring $
                desugarParsedModule ast initialDesugaringState
            writeDebugOutput
                mempty
                    { getFrontendProcessorDebugOutputDesugaring =
                          Just desugaringState
                    }
            inferenceOutput <-
                wrapErrorAndDebugOutput
                    FrontendProcessorErrorInference
                    (\debug ->
                         mempty
                             { getFrontendProcessorDebugOutputInference =
                                   Just debug
                             }) $
                processModule initialInferenceState desugaredAst
            return
                FrontendProcessorOutput
                    { getFrontendProcessorOutputInfix = infixOperators
                    , getFrontendProcessorOutputDesugaring = desugaringState
                    , getFrontendProcessorOutputInference = inferenceOutput
                    }
