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
    , ExplicitProcessorError(..)
    , ModuleExports(..)
    , Explicit(..)
    , Implicit(..)
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
import Frontend.Module.Base
import Frontend.Module.Export.Processor
import Frontend.Syntax.Position (withDummyLocation)
import Frontend.Syntax.Processor
import Util.Debug

-- | A type of errors which can be encountered during processing source files
data FrontendProcessorError
    = FrontendProcessorErrorSyntax SyntaxProcessorError
    | FrontendProcessorErrorDesugaring DesugaringError
    | FrontendProcessorErrorInference InferenceProcessorError
    | FrontendProcessorErrorExport ExplicitProcessorError
    deriving (Eq, Show)

-- | An output of processing sources files
data FrontendProcessorOutput = FrontendProcessorOutput
    { getFrontendProcessorOutputInference :: InferenceProcessorOutput -- ^ Output of inference
    , getFrontendProcessorOutputExports :: ModuleExports -- ^ Exports of a module
    } deriving (Eq, Show)

-- | An empty output
emptyFrontendProcessorOutput :: FrontendProcessorOutput
emptyFrontendProcessorOutput =
    FrontendProcessorOutput
        { getFrontendProcessorOutputInference = defaultInferenceProcessorOutput
        , getFrontendProcessorOutputExports = emptyModuleExports
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

-- | Processes a source file
processSourceFile ::
       ModuleImports
    -> String
    -> TokenStream
    -> ( Either FrontendProcessorError FrontendProcessorOutput
       , FrontendProcessorDebugOutput)
processSourceFile imports fileName stream =
    runWithDebugOutput $ do
        let (desugaringState, inferenceState) = buildStates imports
        ast <-
            wrapErrorAndDebugOutput
                FrontendProcessorErrorSyntax
                (\debug ->
                     mempty {getFrontendProcessorDebugOutputSyntax = Just debug}) $
            processModuleSyntax fileName stream
        desugaredAst <-
            wrapErrorAndDebugOutput
                FrontendProcessorErrorDesugaring
                (\debug ->
                     mempty
                         { getFrontendProcessorDebugOutputDesugaring =
                               Just debug
                         }) $
            desugarParsedModule desugaringState HM.empty ast
        inferenceOutput <-
            wrapErrorAndDebugOutput
                FrontendProcessorErrorInference
                (\debug ->
                     mempty
                         {getFrontendProcessorDebugOutputInference = Just debug}) $
            processModule inferenceState desugaredAst
        exports <-
            wrapEither FrontendProcessorErrorExport $
            processModuleExports desugaredAst inferenceOutput
        return
            FrontendProcessorOutput
                { getFrontendProcessorOutputInference = inferenceOutput
                , getFrontendProcessorOutputExports = exports
                }

buildStates :: ModuleImports -> (ImportedGroups, InferenceProcessorOutput)
buildStates _ = (builtInImportedGroups, defaultInferenceProcessorOutput)

-- | An empty group of imported definitions
builtInImportedGroups :: ImportedGroups
builtInImportedGroups =
    let buildMap = HM.mapWithKey (\k _ -> [withDummyLocation k])
     in ImportedGroups
            { getImportedGroupsTypes = buildMap defaultKindSignatures
            , getImportedGroupsExpressions =
                  buildMap $ defaultConstructors `HM.union` defaultExpressions
            }
