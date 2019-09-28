{- |
Module      :  Frontend.Inference.Kind.Processor
Description :  Functions for kind inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of kind inference
-}
module Frontend.Inference.Kind.Processor
    ( inferKinds
    , KindProcessorDebugOutput(..)
    , KindInferenceEnvironmentItem(..)
    , KindProcessorError(..)
    ) where

import Control.Applicative ((<|>))

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.InferenceProcessor
import qualified Frontend.Inference.Kind.Ast as A
import Frontend.Inference.Kind.Base
import Frontend.Inference.Kind.Checker
import Frontend.Inference.Signature
import Util.Debug

type KindProcessor = WithDebugOutput KindProcessorError KindProcessorDebugOutput

-- | Errors which can be encountered during type processing
data KindProcessorError
    = KindProcessorErrorInference InferenceError -- ^ Kind inference error
    | KindProcessorErrorInstanceCheck InferenceError -- ^ Instance check error
    | KindProcessorErrorAstCheck InferenceError -- ^ Signatures check error
    deriving (Eq, Show)

-- | A type of debug output of kind inference
data KindProcessorDebugOutput = KindProcessorDebugOutput
    { getKindProcessorDebugOutputInference :: Maybe (InferenceDebugOutput KindInferenceEnvironmentItem TypeConstructorSignature)
    , getKindProcessorDebugOutputInstances :: Maybe (SingleGroupInferenceDebugOutput [F.Instance] [()])
    , getKindProcessorDebugOutputCheck :: Maybe [SingleGroupInferenceDebugOutput F.TypeSignature TypeConstructorSignature]
    } deriving (Eq, Show)

instance Semigroup KindProcessorDebugOutput where
    KindProcessorDebugOutput i1 in1 c1 <> KindProcessorDebugOutput i2 in2 c2 =
        KindProcessorDebugOutput (i1 <|> i2) (in1 <|> in2) (c1 <|> c2)

instance Monoid KindProcessorDebugOutput where
    mempty = KindProcessorDebugOutput mempty mempty mempty

-- | Infers kinds of types in a module, checks instance declarations and type expressions
inferKinds ::
       Signatures TypeConstructorSignature
    -> F.Module
    -> ( Either KindProcessorError ( Signatures TypeConstructorSignature
                                   , A.AstWithKinds)
       , KindProcessorDebugOutput)
inferKinds signatures module' =
    runWithDebugOutput (processModule signatures module')

processModule ::
       Signatures TypeConstructorSignature
    -> F.Module
    -> KindProcessor (Signatures TypeConstructorSignature, A.AstWithKinds)
processModule initialSignatures module' = do
    inferredSignatures <-
        wrapErrorAndDebugOutput
            KindProcessorErrorInference
            (\debug ->
                 mempty {getKindProcessorDebugOutputInference = Just debug}) $
        inferKindsOfModule initialSignatures module'
    let allSignatures = initialSignatures <> inferredSignatures
    _ <-
        wrapErrorAndDebugOutput
            KindProcessorErrorInstanceCheck
            (\debug ->
                 mempty {getKindProcessorDebugOutputInstances = Just debug}) $
        checkKindsOfExpressions allSignatures (F.getModuleInstances module')
    ast <-
        wrapErrorAndDebugOutput
            KindProcessorErrorAstCheck
            (\debug -> mempty {getKindProcessorDebugOutputCheck = Just debug}) $
        checkModule allSignatures module'
    return (inferredSignatures, ast)
