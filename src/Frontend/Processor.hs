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
    ) where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Checking.Base (ImportedGroups(..))
import Frontend.Desugaring.Processor
import Frontend.Inference.BuiltIns
import Frontend.Inference.Processor
import Frontend.Syntax.Position (withDummyLocation)
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
    { getFrontendProcessorOutputInference :: InferenceProcessorOutput -- ^ Output of inference
    } deriving (Eq, Show)

-- | An empty output
emptyFrontendProcessorOutput :: FrontendProcessorOutput
emptyFrontendProcessorOutput =
    FrontendProcessorOutput
        {getFrontendProcessorOutputInference = defaultInferenceProcessorOutput}

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

-- | Processes a source file
processSourceFile ::
       FrontendProcessorOutput
    -> String
    -> TokenStream
    -> ( Either FrontendProcessorError FrontendProcessorOutput
       , FrontendProcessorDebugOutput)
processSourceFile initialState fileName stream
    | FrontendProcessorOutput {getFrontendProcessorOutputInference = initialInferenceState} <-
         initialState =
        runWithDebugOutput $ do
            ast <-
                wrapErrorAndDebugOutput
                    FrontendProcessorErrorSyntax
                    (\debug ->
                         mempty
                             { getFrontendProcessorDebugOutputSyntax =
                                   Just debug
                             }) $
                processModuleSyntax fileName stream
            desugaredAst <-
                wrapErrorAndDebugOutput
                    FrontendProcessorErrorDesugaring
                    (\debug ->
                         mempty
                             { getFrontendProcessorDebugOutputDesugaring =
                                   Just debug
                             }) $
                desugarParsedModule builtInImportedGroups HM.empty ast
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
                    {getFrontendProcessorOutputInference = inferenceOutput}

-- | An empty group of imported definitions
builtInImportedGroups :: ImportedGroups
builtInImportedGroups =
    let buildMap = HM.mapWithKey (\k _ -> [withDummyLocation k])
     in ImportedGroups
            { getImportedGroupsTypes = buildMap defaultKindSignatures
            , getImportedGroupsExpressions =
                  buildMap $ defaultConstructors `HM.union` defaultExpressions
            }
