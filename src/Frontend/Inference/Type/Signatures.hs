{- |
Module      :  Frontend.Inference.Type.Signatures
Description :  Functions for signatures checking
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Function for signatures checking
-}
module Frontend.Inference.Type.Signatures where

import Data.Bifunctor (first)

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Constraint
import Frontend.Inference.Kind.Equalities
import Frontend.Inference.Kind.ProcessorBase (Signatures)
import Frontend.Inference.Kind.Solver
import Frontend.Inference.Signature
import Frontend.Inference.TypeSynonyms.Expand
import Frontend.Inference.TypeSynonyms.Processor (TypeSynonymSignatures)
import Frontend.Inference.Unification
import Frontend.Syntax.Position (WithLocation(..))

-- | A type of errors which can be encountered during inference of a signature
data TypeSignatureInferrenceError
    = TypeSignatureInferrenceErrorEqualityGeneration EqualityGenerationError -- ^ An error of equality generation
    | TypeSignatureInferrenceErrorUnification UnificationError -- ^ An unification error
    | TypeSignatureInferrenceErrorTypeSynonyms TypeSynonymsExpandingError -- ^ A type synonym expanding error
    deriving (Eq, Show)

-- | Infers kind and sort for a type signature, and expands type synonyms
inferTypeSignature ::
       Signatures
    -> TypeSynonymSignatures
    -> F.TypeSignature
    -> Either TypeSignatureInferrenceError TypeSignature
inferTypeSignature signatures typeSynonymSignatures signature = do
    let generatedEqualities =
            runEqualitiesGenerator
                (generateEqualitiesForSignature signature)
                signatures
    -- Generate equalities for a signature
    (params, equalities) <-
        first TypeSignatureInferrenceErrorEqualityGeneration generatedEqualities
    -- Solve equalities
    let solvedEqualites = solveEqualitiesAndApplySolution equalities params
    (TypeConstructorSignature { getTypeConstructorSignatureSort = sort
                              , getTypeConstructorSignatureKindParams = kindParams
                              , getTypeConstructorSignatureKind = kind
                              , getTypeConstructorSignatureTypeParams = typeParams
                              }, _) <-
        first TypeSignatureInferrenceErrorUnification solvedEqualites
    -- Expand type synonyms in the type
    let expandedType =
            expandTypeSynonyms
                (F.getTypeSignatureType signature)
                typeSynonymSignatures
    finalType <- first TypeSignatureInferrenceErrorTypeSynonyms expandedType
    -- Expand type synonyms in the context
    let expandedContext =
            mapM (expandConstraint typeSynonymSignatures) $
            F.getTypeSignatureContext signature
    finalContext <-
        first TypeSignatureInferrenceErrorTypeSynonyms expandedContext
    return
        TypeSignature
            { getTypeSignatureSort = sort
            , getTypeSignatureKindParams = kindParams
            , getTypeSignatureKind = kind
            , getTypeSignatureTypeParams = typeParams
            , getTypeSignatureType = finalType
            , getTypeSignatureContext = finalContext
            }

-- | Generates equalities for a signature
generateEqualitiesForSignature ::
       F.TypeSignature -> EqualityGenerator TypeConstructorSignature
generateEqualitiesForSignature signature = do
    signatureWithParams@(TypeSignatureWithParams _ params) <-
        addParamsToTypeSignature signature
    generateEqualities signatureWithParams
    return params

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
