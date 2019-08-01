{- |
Module      :  Frontend.Inference.Type.Signatures
Description :  Functions for signatures checking
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Function for signatures checking
-}
module Frontend.Inference.Type.Signatures where

import Data.Bifunctor (first)
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
     in inferTypeSignatures' signatures typeSynonyms expressionSignatures

-- | Describes inference of kinds of explicit type signatures
signatureKindInferenceDescriptor ::
       SingleGroupInferenceDescriptor (Signatures F.TypeSignature) TypeConstructorSignature
signatureKindInferenceDescriptor =
    SingleGroupInferenceDescriptor
        { getSingleGroupInferenceDescriptorEqualitiesBuilder =
              generateEqualitiesForSignatures
        , getSingleGroupInferenceDescriptorApplySolution = applyKindSolutionAndSetTypeVariables
        }

-- | Infers kinds of explicit type signatures
inferTypeSignatures' ::
       Signatures TypeConstructorSignature
    -> TypeSynonymSignatures
    -> Signatures F.TypeSignature
    -> ( Either InferenceError (Signatures TypeSignature)
       , SingleGroupInferenceDebugOutput)
inferTypeSignatures' signatures typeSynonymSignatures signaturesMap =
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
        (result, debugOutput) = runSingleGroupInferenceProcessor' single
        expandSingle (name, typeSignature) = do
            let sig = fromJust $ HM.lookup name signaturesMap
            expanded <-
                first InferenceErrorSynonyms $
                expandSignatureType typeSynonymSignatures sig typeSignature
            return (name, expanded)
     in ( do (output, _, _) <- result
             HM.fromList <$> mapM expandSingle (HM.toList output)
        , debugOutput)

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
        F.ConstraintType class' type' params ->
            ConstraintType (getValue class') (getValue type') <$>
            mapM (`expandTypeSynonyms` signatures) params
