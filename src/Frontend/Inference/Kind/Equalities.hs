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
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)

import Frontend.Desugaring.Final.Ast
import Frontend.Inference.Equalities
import Frontend.Inference.InferenceProcessor
import Frontend.Inference.Kind.Environment
import Frontend.Inference.Signature hiding
    ( Constraint(..)
    , Type(..)
    , TypeSignature(..)
    )
import Frontend.Syntax.Position

-- | Generates equalities for a single group
generateEqualitiesForGroup ::
       KindInferenceEnvironment
    -> EqualitiesGenerator (Signatures (TypeConstructorSignature, [Ident]))
generateEqualitiesForGroup items = do
    let createSignaturePair name = (\sig -> (name, sig)) <$> createSignature
    signatures <- HM.fromList <$> mapM createSignaturePair (HM.keys items)
    withTypeConstructors signatures $
        hashMapM generateEqualitiesAndSignature items

-- | Creates a type constructor signatures, using provided type parameters
createSignature :: EqualitiesGenerator TypeConstructorSignature
createSignature = do
    (resultKind, resultSort) <- createNewKindVariable
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
    -> EqualitiesGenerator (Kind, Sort)
    -> TypeConstructorSignature
    -> EqualitiesGenerator (TypeConstructorSignature, [Ident])
generateEqualitiesUsingSignature params generator signature = do
    kindParams <- createNewKindVariables params
    (genKind, genSort) <- withKindVariables kindParams generator
    let (kinds, sorts) = unzip $ map snd kindParams
        resultKind = foldr KindFunction genKind kinds
    writeSortSquare $ genSort : sorts
    ((expectedKind, expectedSort), _) <- specialiseDataTypeSignature signature
    writeKindEqualities [(resultKind, expectedKind)]
    writeSortEqualities [(genSort, expectedSort)]
    return (signature, params)

-- | A class of types for which it's posible to generator equalities between
-- | kinds and sorts
class WithEqualities a where
    generateEqualities :: a -> EqualitiesGenerator () -- ^ Generate equalities for an object

instance (WithEqualities a) => WithEqualities (WithLocation a) where
    generateEqualities = generateEqualities . getValue

instance (WithEqualities a) => WithEqualities [a] where
    generateEqualities = mapM_ generateEqualities

-- | Finds a signature and generates equalities
lookupSignatureAndGenerateEqualities ::
       WithLocation Ident
    -> [WithLocation Ident]
    -> EqualitiesGenerator (Kind, Sort)
    -> EqualitiesGenerator (TypeConstructorSignature, [Ident])
lookupSignatureAndGenerateEqualities name params generator =
    let getSignature =
            fromJust . HM.lookup (getValue name) . getTypeConstructorSignatures
     in asks getSignature >>=
        generateEqualitiesUsingSignature (map getValue params) generator

-- | A class of types which have both equalities and signatures
class WithEqualitiesAndSignature a where
    generateEqualitiesAndSignature ::
           a -> EqualitiesGenerator (TypeConstructorSignature, [Ident])

instance WithEqualitiesAndSignature KindInferenceEnvironmentItem where
    generateEqualitiesAndSignature item =
        case item of
            KindInferenceEnvironmentItemTypeSynonym ts ->
                generateEqualitiesAndSignature ts
            KindInferenceEnvironmentItemDataType d ->
                generateEqualitiesAndSignature d
            KindInferenceEnvironmentItemClass c ->
                generateEqualitiesAndSignature c

instance WithEqualitiesAndSignature TypeSynonym where
    generateEqualitiesAndSignature TypeSynonym { getTypeSynonymName = name
                                               , getTypeSynonymParams = params
                                               , getTypeSynonymType = type'
                                               } =
        lookupSignatureAndGenerateEqualities name params $
        generateTypeEqualities type'

instance WithEqualitiesAndSignature DataType where
    generateEqualitiesAndSignature DataType { getDataTypeName = name
                                            , getDataTypeParams = params
                                            , getDataTypeContext = context
                                            , getDataTypeConstructors = constructors
                                            } =
        lookupSignatureAndGenerateEqualities name params $ do
            generateEqualities context
            generateEqualities $ map snd constructors
            return (KindStar, SortSquare)

instance WithEqualitiesAndSignature Class where
    generateEqualitiesAndSignature Class { getClassName = name
                                         , getClassParam = param
                                         , getClassContext = context
                                         , getClassMethods = methods
                                         } =
        lookupSignatureAndGenerateEqualities name [param] $ do
            generateEqualities context
            generateEqualities . map snd $ HM.toList methods
            return (KindStar, SortSquare)

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
                    ConstraintAppliedParam class' param params ->
                        makePseudoType
                            class'
                            (withDummyLocation $
                             TypeApplication
                                 (withDummyLocation $ TypeVar param)
                                 params)
         in void $ generateTypeEqualities pseudoType

instance WithEqualities SimpleConstraint where
    generateEqualities (SimpleConstraint class' param) =
        generateEqualities (ConstraintParam class' param)

instance WithEqualities Constructor where
    generateEqualities Constructor {getConstructorArgs = args} = do
        argKinds <- mapM generateTypeEqualities args
        let (kinds, sorts) = unzip argKinds
        -- Kind of every argument is *, and sort is []
        writeKindStar kinds
        writeSortSquare sorts

instance WithEqualities Method where
    generateEqualities = generateEqualities . getMethodType

instance WithEqualities TypeSignature where
    generateEqualities = void . generateEqualitiesAndSignature

-- | Collect equalities between kinds in a type
generateTypeEqualities :: WithLocation Type -> EqualitiesGenerator (Kind, Sort)
generateTypeEqualities type' =
    case getValue type' of
        TypeVar name -> lookupKindVariable name
        TypeConstr name -> fst <$> lookupKindOfType name
        TypeFunction from to -> do
            (fromKind, fromSort) <- generateTypeEqualities from
            (toKind, toSort) <- generateTypeEqualities to
            -- Kind of (->) is (* -> * -> *)
            writeKindEqualities [(fromKind, KindStar), (toKind, KindStar)]
            -- We expect each argument to be fully specialised
            writeSortSquare [fromSort, toSort]
            return (KindStar, SortSquare)
        TypeApplication func args -> do
            (funcKind, funcSort) <- generateTypeEqualities func
            argsResolved <- mapM generateTypeEqualities (NE.toList args)
            (resultKind, resultSort) <- createNewKindVariable
            let (kinds, sorts) = unzip argsResolved
                expectedKind = foldr KindFunction resultKind kinds
            writeKindEqualities [(funcKind, expectedKind)]
            -- Each argument should be fully specialised
            writeSortSquare $ funcSort : resultSort : sorts
            return (resultKind, SortSquare)

instance WithEqualitiesAndSignature TypeSignature where
    generateEqualitiesAndSignature TypeSignature { getTypeSignatureType = type'
                                                 , getTypeSignatureContext = context
                                                 } = do
        signature <- createSignature
        let params =
                HS.toList . HS.unions $
                getFreeTypeVariables type' :
                map getFreeTypeVariablesOfConstraint context
            generator = do
                generateEqualities context
                generateTypeEqualities type'
        generateEqualitiesUsingSignature params generator signature

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

-- | Gets free type variables of a constraint
getFreeTypeVariablesOfConstraint :: WithLocation Constraint -> HS.HashSet Ident
getFreeTypeVariablesOfConstraint constraint =
    case getValue constraint of
        ConstraintParam _ param -> HS.singleton (getValue param)
        ConstraintAppliedParam _ param args ->
            HS.unions $
            HS.singleton (getValue param) :
            map getFreeTypeVariables (NE.toList args)

-- | Monadically maps values of a monad
hashMapM ::
       (Monad m) => (a -> m b) -> HM.HashMap Ident a -> m (HM.HashMap Ident b)
hashMapM f hashMap =
    let applySecond (p, s) = (\x -> (p, x)) <$> f s
     in HM.fromList <$> mapM applySecond (HM.toList hashMap)
