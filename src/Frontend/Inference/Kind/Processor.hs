{- |
Module      :  Frontend.Inference.Kind.Processor
Description :  Functions for kind inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of kind inference
-}
module Frontend.Inference.Kind.Processor
    ( inferKinds
    , KindInferenceDebugOutput(..)
    , KindInferenceEnvironmentItem(..)
    ) where

import Control.Applicative ((<|>))

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.InferenceProcessor
import qualified Frontend.Inference.Kind.Ast as A
import Frontend.Inference.Kind.Base
import Frontend.Inference.Kind.Checker
import Frontend.Inference.Signature
import Frontend.Inference.Util.Debug

type KindInferenceProcessor
     = WithDebugOutput InferenceError KindInferenceDebugOutput

-- | A type of debug output of kind inference
data KindInferenceDebugOutput = KindInferenceDebugOutput
    { getKindInferenceDebugOutputInference :: Maybe (InferenceDebugOutput KindInferenceEnvironmentItem TypeConstructorSignature)
    , getKindInferenceDebugOutputInstances :: Maybe (SingleGroupInferenceDebugOutput [F.Instance] [()])
    , getKindInferenceDebugOutputCheck :: Maybe [SingleGroupInferenceDebugOutput F.TypeSignature TypeConstructorSignature]
    }

instance Semigroup KindInferenceDebugOutput where
    KindInferenceDebugOutput i1 in1 c1 <> KindInferenceDebugOutput i2 in2 c2 =
        KindInferenceDebugOutput (i1 <|> i2) (in1 <|> in2) (c1 <|> c2)

instance Monoid KindInferenceDebugOutput where
    mempty = KindInferenceDebugOutput mempty mempty mempty

-- | Infers kinds of types in a module, checks instance declarations and type expressions
inferKinds ::
       Signatures TypeConstructorSignature
    -> F.Module
    -> ( Either InferenceError ( Signatures TypeConstructorSignature
                               , A.AstWithKinds)
       , KindInferenceDebugOutput)
inferKinds signatures module' =
    runWithDebugOutput (processModule signatures module')

processModule ::
       Signatures TypeConstructorSignature
    -> F.Module
    -> KindInferenceProcessor ( Signatures TypeConstructorSignature
                              , A.AstWithKinds)
processModule initialSignatures module' = do
    inferredSignatures <-
        wrapDebugOutput
            (\debug ->
                 mempty {getKindInferenceDebugOutputInference = Just debug}) $
        inferKindsOfModule initialSignatures module'
    let allSignatures = initialSignatures <> inferredSignatures
    _ <-
        wrapDebugOutput
            (\debug ->
                 mempty {getKindInferenceDebugOutputInstances = Just debug}) $
        checkKindsOfExpressions allSignatures (F.getModuleInstances module')
    ast <-
        wrapDebugOutput
            (\debug -> mempty {getKindInferenceDebugOutputCheck = Just debug}) $
        checkModule allSignatures module'
    return (inferredSignatures, ast)
