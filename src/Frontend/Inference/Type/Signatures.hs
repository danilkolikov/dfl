{- |
Module      :  Frontend.Inference.Type.Signatures
Description :  Functions for signatures checking
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Function for signatures checking
-}
module Frontend.Inference.Type.Signatures where

import qualified Data.HashMap.Lazy as HM
import Data.Maybe (mapMaybe)

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Base.Common
import Frontend.Inference.Base.DebugOutput
import Frontend.Inference.Signature
import Frontend.Inference.Util.Debug

-- | Infers kinds of explicit type signatures
inferTypeSignatures ::
       Signatures TypeConstructorSignature
    -> Signatures TypeSignature
    -> F.Expressions
    -> ( Either InferenceError (Signatures TypeSignature)
       , SingleGroupInferenceDebugOutput)
inferTypeSignatures signatures typeSynonyms expressions =
    let getSignature (name, F.Expression {F.getExpressionType = maybeType}) =
            (\t -> (name, t)) <$> maybeType
        expressionSignatures =
            HM.fromList . mapMaybe getSignature . HM.toList $ expressions
     in runWithDebugOutput $
        inferTypeSignatures' signatures typeSynonyms expressionSignatures

-- | Infers kinds of explicit type signatures
inferTypeSignatures' ::
       Signatures TypeConstructorSignature
    -> Signatures TypeSignature
    -> Signatures F.TypeSignature
    -> WithDebugOutput InferenceError SingleGroupInferenceDebugOutput (Signatures TypeSignature)
inferTypeSignatures' _ _ _ = error "TODO: rewrite this function"
