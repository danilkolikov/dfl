{- |
Module      :  Frontend.Inference.Type.Signatures
Description :  Functions for signatures checking
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Function for signatures checking
-}
module Frontend.Inference.Type.Signatures where

import Data.Bifunctor (second)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromJust, mapMaybe)

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Base.Common
import Frontend.Inference.Base.DebugOutput
import Frontend.Inference.Base.Descriptor
import Frontend.Inference.Base.SingleGroupProcessor
import Frontend.Inference.Constraint
import Frontend.Inference.Kind.Equalities
import Frontend.Inference.Signature
import Frontend.Inference.Solver
import Frontend.Inference.TypeSynonyms.Expand
import Frontend.Inference.TypeSynonyms.Processor (TypeSynonymSignatures)
import Frontend.Inference.Util.Debug
import Frontend.Inference.Variables
import Frontend.Syntax.Position (WithLocation(..))

-- | Infers kinds of explicit type signatures
inferTypeSignatures ::
       Signatures TypeConstructorSignature
    -> TypeSynonymSignatures
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

-- | Describes inference of kinds of explicit type signatures
signatureKindInferenceDescriptor ::
       SingleGroupInferenceDescriptor (Signatures F.TypeSignature) TypeConstructorSignature ()
signatureKindInferenceDescriptor =
    SingleGroupInferenceDescriptor
        { getSingleGroupInferenceDescriptorEqualitiesBuilder =
              generateEqualitiesForSignatures
        , getSingleGroupInferenceDescriptorApplySolution =
              \def eq sol ->
                  second $ applyKindSolutionAndSetTypeVariables def eq sol
        }

-- | Infers kinds of explicit type signatures
inferTypeSignatures' ::
       Signatures TypeConstructorSignature
    -> TypeSynonymSignatures
    -> Signatures F.TypeSignature
    -> WithDebugOutput InferenceError SingleGroupInferenceDebugOutput (Signatures TypeSignature)
inferTypeSignatures' signatures typeSynonymSignatures signaturesMap = do
    let single =
            inferSingleGroup
                signatureKindInferenceDescriptor
                InferenceEnvironment
                    { getInferenceEnvironmentSignatures = signatures
                    , getInferenceEnvironmentTypeVariables = HM.empty
                    }
                undefined -- recursive call is not used
                signaturesMap
                (HM.keys signaturesMap)
                emptyVariableGeneratorState
    (result, _, _) <- single
    let expandSingle (name, (_, typeSignature)) = do
            let sig = fromJust $ HM.lookup name signaturesMap
            expanded <-
                wrapEither InferenceErrorSynonyms $
                expandSignatureType typeSynonymSignatures sig typeSignature
            return (name, expanded)
    HM.fromList <$> mapM expandSingle (HM.toList result)

-- | Expands explicit type signature
expandSignatureType ::
       TypeSynonymSignatures
    -> F.TypeSignature
    -> TypeConstructorSignature
    -> Either TypeSynonymsExpandingError TypeSignature
expandSignatureType typeSynonymSignatures typeSig constrSig
    | TypeConstructorSignature { getTypeConstructorSignatureSort = sort
                               , getTypeConstructorSignatureKindParams = kindParams
                               , getTypeConstructorSignatureKind = kind
                               , getTypeConstructorSignatureTypeParams = typeParams
                               } <- constrSig
    , F.TypeSignature { F.getTypeSignatureContext = context
                      , F.getTypeSignatureType = type'
                      } <- typeSig = do
        expandedType <- expandTypeSynonyms type' typeSynonymSignatures
        expandedContext <- mapM (expandConstraint typeSynonymSignatures) context
        return
            TypeSignature
                { getTypeSignatureSort = sort
                , getTypeSignatureKindParams = kindParams
                , getTypeSignatureKind = kind
                , getTypeSignatureTypeParams = typeParams
                , getTypeSignatureType = expandedType
                , getTypeSignatureContext = expandedContext
                }

-- | Expands type synonyms in a constraint
expandConstraint ::
       TypeSynonymSignatures
    -> WithLocation F.Constraint
    -> Either TypeSynonymsExpandingError Constraint
expandConstraint signatures constraint =
    case getValue constraint of
        F.ConstraintParam class' param ->
            return $
            ConstraintVariable (getValue class') (TypeVar $ getValue param)
        F.ConstraintAppliedParam class' param params ->
            ConstraintAppliedVariable
                (getValue class')
                (TypeVar $ getValue param) <$>
            mapM (`expandTypeSynonyms` signatures) params
