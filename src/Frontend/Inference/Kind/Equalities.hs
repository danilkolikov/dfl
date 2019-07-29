{- |
Module      :  Frontend.Inference.Kind.Equalities
Description :  Function for generation of equalities
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for generation of equalities between kinds and sorts
-}
module Frontend.Inference.Kind.Equalities where

import Control.Monad (void)
import Control.Monad.Trans.Reader (asks)
import Data.Foldable (asum)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, fromMaybe)

import Frontend.Desugaring.Final.Ast
import Frontend.Inference.Base.Common
import Frontend.Inference.Base.DebugOutput
import Frontend.Inference.Base.Descriptor
import Frontend.Inference.Equalities
import Frontend.Inference.Kind.Environment
import Frontend.Inference.Signature hiding
    ( Constraint(..)
    , Type(..)
    , TypeSignature(..)
    )
import Frontend.Inference.Variables hiding (Type(..))
import Frontend.Syntax.Position

-- | Runs a generator of kind equalities
runKindEqualitiesGenerator ::
       InferenceEqualitiesGenerator a
    -> Signatures TypeConstructorSignature
    -> VariableGeneratorState
    -> ( ( Either InferenceEqualitiesGenerationError (a, Equalities)
         , [InferenceDebugOutput])
       , VariableGeneratorState)
runKindEqualitiesGenerator generator signatures =
    let localEnvironment =
            emptyEqualitiesGeneratorEnvironment
                {getTypeConstructorSignatures = signatures}
     in runEqualitiesGenerator generator localEnvironment []

-- | Generates equalities for a single group
generateEqualitiesForGroup ::
       EqualitiesBuilder Environment (Signatures TypeConstructorSignature)
generateEqualitiesForGroup _ inferenceEnv env items
    | InferenceEnvironment {getInferenceEnvironmentSignatures = signatures} <-
         inferenceEnv =
        runKindEqualitiesGenerator
            (generateEqualitiesForGroup' env items)
            signatures

-- | Generates equalities for a single group
generateEqualitiesForGroup' ::
       Environment
    -> [Ident]
    -> InferenceEqualitiesGenerator (Signatures TypeConstructorSignature)
generateEqualitiesForGroup' env group = do
    let createSignaturePair name = (\sig -> (name, sig)) <$> createSignature
    signatures <- HM.fromList <$> mapM createSignaturePair group
    withTypeConstructors signatures $
        mapM_ (generateEqualitiesForIdent env) group
    return signatures

-- | Generates equalities for a group of signatures
generateEqualitiesForSignatures ::
       EqualitiesBuilder (HM.HashMap Ident TypeSignature) (Signatures TypeConstructorSignature)
generateEqualitiesForSignatures _ inferenceEnv env items
    | InferenceEnvironment {getInferenceEnvironmentSignatures = signatures} <-
         inferenceEnv =
        runKindEqualitiesGenerator
            (generateEqualitiesForTypeSignatures' env items)
            signatures

-- | Generates equalities for a group of signatures
generateEqualitiesForTypeSignatures' ::
       HM.HashMap Ident TypeSignature
    -> [Ident]
    -> InferenceEqualitiesGenerator (Signatures TypeConstructorSignature)
generateEqualitiesForTypeSignatures' env idents = do
    let processSingle name = do
            let signature = fromJust $ HM.lookup name env
            result <- generateEqualitiesForTypeSignature signature
            return (name, result)
    HM.fromList <$> mapM processSingle idents

-- | Creates a type constructor signatures, using provided type parameters
createSignature :: InferenceEqualitiesGenerator TypeConstructorSignature
createSignature =
    liftGen $ do
        resultKind <- generateKindVariable
        resultSort <- generateSortVariable
        return $
            TypeConstructorSignature
                { getTypeConstructorSignatureSort = resultSort
                , getTypeConstructorSignatureKindParams = []
                , getTypeConstructorSignatureKind = resultKind
                , getTypeConstructorSignatureTypeParams = []
                }

-- | Generates equalities using a provided signature
generateEqualitiesUsingSignature ::
       [Ident]
    -> InferenceEqualitiesGenerator Kind
    -> TypeConstructorSignature
    -> InferenceEqualitiesGenerator ()
generateEqualitiesUsingSignature params generator signature = do
    typeParams <- createNewVariables params
    kind <- withTypeVariables typeParams generator
    let resultKind =
            foldr (KindFunction . (\(_, k, _) -> k) . snd) kind typeParams
    expectedKind <- specialiseDataTypeSignature signature
    writeKindEqualities [(resultKind, expectedKind)]

-- | Generates equalities for an ident
generateEqualitiesForIdent ::
       Environment -> Ident -> InferenceEqualitiesGenerator ()
generateEqualitiesForIdent env name =
    let maybeResolveSingle getMap =
            generateEqualities <$> HM.lookup name (getMap env)
        maybeGenerator =
            asum
                [ maybeResolveSingle getTypeSynonyms
                , maybeResolveSingle getDataTypes
                , maybeResolveSingle getClasses
                ]
      -- This error should not occur, because we expect that all idents are
      -- either type synonyms, data types or classes
     in fromMaybe (error $ "Unexpected identifier " ++ show name) maybeGenerator

-- | A class of types for which it's posible to generator equalities between
-- | kinds and sorts
class WithEqualities a where
    generateEqualities :: a -> InferenceEqualitiesGenerator () -- ^ Generate equalities for an object

instance (WithEqualities a) => WithEqualities (WithLocation a) where
    generateEqualities = generateEqualities . getValue

instance (WithEqualities a) => WithEqualities [a] where
    generateEqualities = mapM_ generateEqualities

-- | Finds a signature and generates equalities
lookupSignatureAndGenerateEqualities ::
       WithLocation Ident
    -> [WithLocation Ident]
    -> InferenceEqualitiesGenerator Kind
    -> InferenceEqualitiesGenerator ()
lookupSignatureAndGenerateEqualities name params generator =
    let getSignature =
            fromJust . HM.lookup (getValue name) . getTypeConstructorSignatures
     in asks getSignature >>=
        generateEqualitiesUsingSignature (map getValue params) generator

instance WithEqualities TypeSynonym where
    generateEqualities TypeSynonym { getTypeSynonymName = name
                                   , getTypeSynonymParams = params
                                   , getTypeSynonymType = type'
                                   } =
        lookupSignatureAndGenerateEqualities name params $
        generateTypeEqualities type'

instance WithEqualities DataType where
    generateEqualities DataType { getDataTypeName = name
                                , getDataTypeParams = params
                                , getDataTypeContext = context
                                , getDataTypeConstructors = constructors
                                } =
        lookupSignatureAndGenerateEqualities name params $ do
            generateEqualities context
            generateEqualities $ map snd constructors
            return KindStar

instance WithEqualities Class where
    generateEqualities Class { getClassName = name
                             , getClassParam = param
                             , getClassContext = context
                             , getClassMethods = methods
                             } =
        lookupSignatureAndGenerateEqualities name [param] $ do
            generateEqualities context
            generateEqualities . map snd $ HM.toList methods
            return KindStar

instance WithEqualities Constraint where
    generateEqualities constr =
        let makePseudoType className arg =
                withDummyLocation $
                TypeApplication
                    (withDummyLocation $ TypeConstr className)
                    (arg NE.:| [])
            pseudoType =
                case constr of
                    ConstraintParam class' param ->
                        makePseudoType
                            class'
                            (withDummyLocation $ TypeVar param)
                    ConstraintType class' type' params ->
                        makePseudoType
                            class'
                            (withDummyLocation $
                             TypeApplication
                                 (withDummyLocation $ TypeConstr type')
                                 params)
         in void $ generateTypeEqualities pseudoType

instance WithEqualities SimpleConstraint where
    generateEqualities (SimpleConstraint class' param) =
        generateEqualities (ConstraintParam class' param)

instance WithEqualities Constructor where
    generateEqualities Constructor {getConstructorArgs = args} = do
        argKinds <- mapM generateTypeEqualities args
    -- Kind of every argument is *
        writeKindEqualities $ map (\k -> (k, KindStar)) argKinds

instance WithEqualities Method where
    generateEqualities = generateEqualities . getMethodType

instance WithEqualities TypeSignature where
    generateEqualities = void . generateEqualitiesForTypeSignature

-- | Collect equalities between kinds in a type
generateTypeEqualities :: WithLocation Type -> InferenceEqualitiesGenerator Kind
generateTypeEqualities type' =
    case getValue type' of
        TypeVar name -> lookupKindOfTypeVariable name
        TypeConstr name -> lookupKindOfType name
        TypeFunction from to -> do
            fromKind <- generateTypeEqualities from
            toKind <- generateTypeEqualities to
            -- Kind of (->) is (* -> * -> *)
            writeKindEqualities [(fromKind, KindStar), (toKind, KindStar)]
            return KindStar
        TypeApplication func args -> do
            funcKind <- generateTypeEqualities func
            argsResolved <- mapM generateTypeEqualities args
            resultKind <- liftGen generateKindVariable
            let expectedKind = foldr KindFunction resultKind argsResolved
            writeKindEqualities [(funcKind, expectedKind)]
            return resultKind

-- | Generates equalities for a type signature
generateEqualitiesForTypeSignature ::
       TypeSignature -> InferenceEqualitiesGenerator TypeConstructorSignature
generateEqualitiesForTypeSignature TypeSignature { getTypeSignatureType = type'
                                                 , getTypeSignatureContext = context
                                                 } = do
    signature <- createSignature
    let params = HS.toList . getFreeTypeVariables $ type'
        generator = do
            generateEqualities context
            generateTypeEqualities type'
    generateEqualitiesUsingSignature params generator signature
    return signature

-- | Get variables of a type
getFreeTypeVariables :: WithLocation Type -> HS.HashSet Ident
getFreeTypeVariables aType =
    case getValue aType of
        TypeVar wl -> HS.singleton (getValue wl)
        TypeConstr _ -> HS.empty
        TypeFunction from to ->
            getFreeTypeVariables from `HS.union` getFreeTypeVariables to
        TypeApplication func args ->
            HS.unions . map getFreeTypeVariables $ func : NE.toList args
