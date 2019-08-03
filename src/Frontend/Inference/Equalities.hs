{- |
Module      :  Frontend.Inference.Equalities
Description :  Function for generation of equalities between types, kinds and sorts
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for generation of equalities between types, kinds and sorts
-}
module Frontend.Inference.Equalities where

import Control.Monad ((>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.Trans.State.Lazy (StateT, modify, runStateT)
import Control.Monad.Trans.Writer.Lazy (WriterT, runWriterT, tell)
import Data.Bifunctor (first, second)
import qualified Data.HashMap.Lazy as HM

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Signature
import Frontend.Inference.Substitution
import Frontend.Inference.Variables hiding (Type(..))
import Frontend.Syntax.Position

-- | A system of equalities between types, kinds and sorts
data Equalities = Equalities
    { getTypeEqualities :: [(Type, Type)]
    , getKindEqualities :: [(Kind, Kind)]
    , getSortEqualities :: [(Sort, Sort)]
    , getHasKindEqualities :: [(Type, Kind)]
    , getHasSortEqualities :: [(Kind, Sort)]
    } deriving (Eq, Show)

instance Semigroup Equalities where
    Equalities t1 k1 s1 hk1 hs1 <> Equalities t2 k2 s2 hk2 hs2 =
        Equalities (t1 <> t2) (k1 <> k2) (s1 <> s2) (hk1 <> hk2) (hs1 <> hs2)

instance Monoid Equalities where
    mempty = Equalities mempty mempty mempty mempty mempty

-- | Types, kinds and sorts of type variables
type TypeVariables = HM.HashMap Ident (Type, Kind, Sort)

-- | Kinds and sorts of kind variables
type KindVariables = HM.HashMap Ident (Kind, Sort)

-- | A local environment of inference
data EqualitiesGeneratorEnvironment = EqualitiesGeneratorEnvironment
    { getTypeConstructorSignatures :: HM.HashMap F.Ident TypeConstructorSignature -- ^ Signatures of type synonyms
    , getExpressionSignatures :: HM.HashMap F.Ident TypeSignature -- ^ Signatures of expressions or constructors
    , getTypeVariables :: TypeVariables -- ^ Types of variables
    , getKindVariables :: KindVariables -- ^ Kinds of variables
    }

-- | An empty environment of inference
emptyEqualitiesGeneratorEnvironment :: EqualitiesGeneratorEnvironment
emptyEqualitiesGeneratorEnvironment =
    EqualitiesGeneratorEnvironment
        { getTypeConstructorSignatures = HM.empty
        , getExpressionSignatures = HM.empty
        , getTypeVariables = HM.empty
        , getKindVariables = HM.empty
        }

-- | Defines new type constructors
defineTypeConstructors ::
       HM.HashMap F.Ident TypeConstructorSignature
    -> EqualitiesGeneratorEnvironment
    -> EqualitiesGeneratorEnvironment
defineTypeConstructors m env =
    env
        { getTypeConstructorSignatures =
              getTypeConstructorSignatures env `HM.union` m
        }

-- | Defines new expressions
defineExpressions ::
       HM.HashMap F.Ident TypeSignature
    -> EqualitiesGeneratorEnvironment
    -> EqualitiesGeneratorEnvironment
defineExpressions m env =
    env {getExpressionSignatures = getExpressionSignatures env `HM.union` m}

-- | Defines new type variables
defineTypeVariables ::
       [(Ident, (Type, Kind, Sort))]
    -> EqualitiesGeneratorEnvironment
    -> EqualitiesGeneratorEnvironment
defineTypeVariables m env =
    env {getTypeVariables = getTypeVariables env `HM.union` HM.fromList m}

-- | Defines new type variables
defineKindVariables ::
       [(Ident, (Kind, Sort))]
    -> EqualitiesGeneratorEnvironment
    -> EqualitiesGeneratorEnvironment
defineKindVariables m env =
    env {getKindVariables = getKindVariables env `HM.union` HM.fromList m}

-- | Errors which may be encountered during generation of equalities of a dependency group
data EqualitiesGenerationError e
    = EqualitiesGenerationErrorUnknownName (WithLocation Ident) -- ^ Unknown name
    | EqualitiesGenerationErrorUnknownType (WithLocation Ident) -- ^ Unknown type
    | EqualitiesGenerationErrorNested e -- ^ A nested error
    deriving (Eq, Show)

-- | A generator of equalities in a dependency group
type EqualitiesGenerator d e
     = ReaderT EqualitiesGeneratorEnvironment (WriterT Equalities (ExceptT (EqualitiesGenerationError e) (StateT d VariableGenerator)))

-- | Runs the equality generator
runEqualitiesGenerator ::
       EqualitiesGenerator d e a
    -> EqualitiesGeneratorEnvironment
    -> d
    -> VariableGeneratorState
    -> ( (Either (EqualitiesGenerationError e) (a, Equalities), d)
       , VariableGeneratorState)
runEqualitiesGenerator generator environment debugState varState =
    let writer = runReaderT generator environment
        exceptT = runWriterT writer
        stateT = runExceptT exceptT
        variables = runStateT stateT debugState
     in runVariableGenerator variables varState

-- | Saves equalities between types
writeTypeEqualities :: [(Type, Type)] -> EqualitiesGenerator d e ()
writeTypeEqualities typeEqualities =
    lift $ tell mempty {getTypeEqualities = typeEqualities}

-- | Saves equalities between kinds
writeKindEqualities :: [(Kind, Kind)] -> EqualitiesGenerator d e ()
writeKindEqualities kindEqualities =
    lift $ tell mempty {getKindEqualities = kindEqualities}

-- | Saves equalities between sorts
writeSortEqualities :: [(Sort, Sort)] -> EqualitiesGenerator d e ()
writeSortEqualities sortEqualities =
    lift $ tell mempty {getSortEqualities = sortEqualities}

-- | Saves "sorting" relation between types and kinds
writeHasKindEqualities :: [(Type, Kind)] -> EqualitiesGenerator d e ()
writeHasKindEqualities equalities =
    lift $ tell mempty {getHasKindEqualities = equalities}

-- | Saves "sorting" relation between kinds and sorts
writeHasSortEqualities :: [(Kind, Sort)] -> EqualitiesGenerator d e ()
writeHasSortEqualities equalities =
    lift $ tell mempty {getHasSortEqualities = equalities}

-- | Writes equalities "kind = *" for all provided kinds
writeKindStar :: [Kind] -> EqualitiesGenerator d e ()
writeKindStar =
    writeKindEqualities . map (\x -> (x, KindStar)) . filter (/= KindStar)

-- | Writes equalities "sort = []" for all provided sorts
writeSortSquare :: [Sort] -> EqualitiesGenerator d e ()
writeSortSquare =
    writeSortEqualities . map (\x -> (x, SortSquare)) . filter (/= SortSquare)

-- | Raise an error
raiseError :: EqualitiesGenerationError e -> EqualitiesGenerator d e a
raiseError = lift . lift . throwE

-- | Wraps a nested error
wrapNestedError :: Either e a -> EqualitiesGenerator d e a
wrapNestedError = lift . lift . except . first EqualitiesGenerationErrorNested

-- | Modifes a debug output
modifyDebugOutput :: (d -> d) -> EqualitiesGenerator d e ()
modifyDebugOutput = lift . lift . lift . modify

-- | Lifts a variable generator
liftGen :: VariableGenerator a -> EqualitiesGenerator d e a
liftGen = lift . lift . lift . lift

-- | Defines new kind variables
defineNewKindVariables :: [(Kind, Sort)] -> EqualitiesGenerator d e ()
defineNewKindVariables = writeHasSortEqualities

-- | Defines provided type variables
defineNewTypeVariables :: [(Type, Kind, Sort)] -> EqualitiesGenerator d e ()
defineNewTypeVariables vars =
    let hasKind = map (\(t, k, _) -> (t, k)) vars
        hasSort = map (\(_, k, s) -> (k, s)) vars
     in writeHasKindEqualities hasKind >> writeHasSortEqualities hasSort

-- | Creates new variables, using provided variable and equalities generators
createNewVariables' ::
       VariableGenerator a
    -> ([a] -> EqualitiesGenerator d e ())
    -> [Ident]
    -> EqualitiesGenerator d e [(Ident, a)]
createNewVariables' generate define idents = do
    let createVariables name = (\v -> (name, v)) <$> generate
    vars <- liftGen $ mapM createVariables idents
    define (map snd vars)
    return vars

-- | Creates new kind and sort variables and saves relations between them
createNewKindVariables ::
       [Ident] -> EqualitiesGenerator d e [(Ident, (Kind, Sort))]
createNewKindVariables =
    createNewVariables' generateKSVariables defineNewKindVariables

-- | Creates new type, kind and sort variables for idents and writes relations between them
createNewTypeVariables ::
       [Ident] -> EqualitiesGenerator d e [(Ident, (Type, Kind, Sort))]
createNewTypeVariables =
    createNewVariables' generateTKSVariables defineNewTypeVariables

-- | Creates new variable using provided variable and equalities generators
createNewVariable' ::
       VariableGenerator a
    -> ([a] -> EqualitiesGenerator d e ())
    -> EqualitiesGenerator d e a
createNewVariable' generate define = do
    var <- liftGen generate
    define [var]
    return var

-- | Creates new kind and sort variable and saves relations between them
createNewKindVariable :: EqualitiesGenerator d e (Kind, Sort)
createNewKindVariable =
    createNewVariable' generateKSVariables defineNewKindVariables

-- | Creates new type, kind and sort variable and saves relations between them
createNewTypeVariable :: EqualitiesGenerator d e (Type, Kind, Sort)
createNewTypeVariable =
    createNewVariable' generateTKSVariables defineNewTypeVariables

-- | Executes generator with provided type constructors
withTypeConstructors ::
       HM.HashMap Ident TypeConstructorSignature
    -> EqualitiesGenerator d e a
    -> EqualitiesGenerator d e a
withTypeConstructors constructors = local (defineTypeConstructors constructors)

-- | Executes generator with provided expressions
withExpressions ::
       HM.HashMap Ident TypeSignature
    -> EqualitiesGenerator d e a
    -> EqualitiesGenerator d e a
withExpressions expressions = local (defineExpressions expressions)

-- | Executes the generator with provided type variables
withTypeVariables ::
       [(Ident, (Type, Kind, Sort))]
    -> EqualitiesGenerator d e a
    -> EqualitiesGenerator d e a
withTypeVariables params = local (defineTypeVariables params)

-- | Executes the generator with provided kind variables
withKindVariables ::
       [(Ident, (Kind, Sort))]
    -> EqualitiesGenerator d e a
    -> EqualitiesGenerator d e a
withKindVariables params = local (defineKindVariables params)

-- | Finds kind of a variable
lookupKindVariable :: WithLocation Ident -> EqualitiesGenerator d e (Kind, Sort)
lookupKindVariable name = do
    kindMapping <- asks getKindVariables
    case HM.lookup (getValue name) kindMapping of
        Just res -> return res
        Nothing -> raiseError $ EqualitiesGenerationErrorUnknownName name

-- | Finds the signature of a data type, type synonym or a class
lookupTypeConstructorSignature ::
       WithLocation Ident -> EqualitiesGenerator d e TypeConstructorSignature
lookupTypeConstructorSignature name = do
    signatures <- asks getTypeConstructorSignatures
    case HM.lookup (getValue name) signatures of
        Just signature -> return signature
        Nothing -> raiseError $ EqualitiesGenerationErrorUnknownType name

-- | Finds the kind of a type constructor
lookupKindOfType :: WithLocation Ident -> EqualitiesGenerator d e (Kind, Sort)
lookupKindOfType =
    lookupTypeConstructorSignature >=> specialiseDataTypeSignature

-- | Finds the type of a variable
lookupTypeVariable ::
       WithLocation Ident -> EqualitiesGenerator d e (Maybe (Type, Kind, Sort))
lookupTypeVariable name = do
    typeMapping <- asks getTypeVariables
    return $ HM.lookup (getValue name) typeMapping

-- | Finds the signature of an expression
lookupExpressionSignature ::
       WithLocation Ident -> EqualitiesGenerator d e TypeSignature
lookupExpressionSignature name = do
    signatures <- asks getExpressionSignatures
    case HM.lookup (getValue name) signatures of
        Just signature -> return signature
        Nothing -> raiseError $ EqualitiesGenerationErrorUnknownName name

-- | Finds the type of a variable or an expression
lookupTypeOfVariableOrExpression ::
       WithLocation Ident -> EqualitiesGenerator d e (Type, Kind, Sort)
lookupTypeOfVariableOrExpression name = do
    maybeType <- lookupTypeVariable name
    case maybeType of
        Just res -> return res
        Nothing ->
            lookupExpressionSignature name >>= specialiseExpressionSignature

-- | Generates new kind and sort variables for an ident
specialiseKindVariable :: Ident -> EqualitiesGenerator d e (Ident, (Kind, Sort))
specialiseKindVariable name = (\x -> (name, x)) <$> createNewKindVariable

-- | Generates new type and kind variables for an ident
specialiseTypeVariable ::
       Ident -> EqualitiesGenerator d e (Ident, (Type, Kind, Sort))
specialiseTypeVariable name = (\x -> (name, x)) <$> createNewTypeVariable

-- | Specialises the signature of a kind constructor
specialiseKindConstructorSignature ::
       (WithSort a, WithKindParams a) => a -> EqualitiesGenerator d e Sort
specialiseKindConstructorSignature = return . getFullSort -- Nothing to specialise here

-- | Specialises the signature of a type constructor
specialiseTypeConstructorSignature ::
       (WithKind a, WithKindParams a, WithTypeParams a)
    => Sort
    -> a
    -> EqualitiesGenerator d e (Kind, Sort)
specialiseTypeConstructorSignature expectedSort sig = do
    specialisedKind <- mapM (specialiseKindVariable . fst) (getKindParams sig)
    -- Ensure that sorts match
    resSort <- liftGen generateSortVariable
    let resultParamSorts = map (snd . snd) specialisedKind
        resultSort = foldr SortFunction resSort resultParamSorts
    writeSortEqualities [(resultSort, expectedSort)]
    -- Substitute kinds with new parameters
    let kindSubstitution = HM.fromList $ map (second fst) specialisedKind
        resultKind = substitute kindSubstitution (getFullKind sig)
    -- Saves information about the sort
    writeHasSortEqualities [(resultKind, resSort)]
    return (resultKind, resSort)

-- | Specialises the signature of a type
specialiseTypeSignature ::
       (WithType a, WithTypeParams a)
    => (Kind, Sort)
    -> a
    -> EqualitiesGenerator d e (Type, Kind, Sort)
specialiseTypeSignature (expectedKind, expectedSort) sig = do
    specialisedType <- mapM (specialiseTypeVariable . fst) (getTypeParams sig)
    -- Ensure that kinds match
    resKind <- liftGen generateKindVariable
    let resultParamKinds = map ((\(_, k, _) -> k) . snd) specialisedType
        resultKind = foldr KindFunction resKind resultParamKinds
    writeKindEqualities [(resultKind, expectedKind)]
    -- Substitute types with new parameters
    let typeSubstitution =
            HM.fromList $ map (second (\(t, _, _) -> t)) specialisedType
        resultType = substitute typeSubstitution (getType sig)
    -- Saves information about the kind
    writeHasKindEqualities [(resultType, resKind)]
    return (resultType, resKind, expectedSort)

-- | Specialises the kind of a data type. Function creates new kind and sort
-- | variables and specialises the kind with them.
specialiseDataTypeSignature ::
       (WithSort a, WithKindParams a, WithKind a, WithTypeParams a)
    => a
    -> EqualitiesGenerator d e (Kind, Sort)
specialiseDataTypeSignature signature = do
    sort <- specialiseKindConstructorSignature signature
    specialiseTypeConstructorSignature sort signature

-- | Specialises the type of an expression. Function creates new type, kind and sort
-- | variables and specialises the type with them.
specialiseExpressionSignature ::
       (WithSort a, WithKindParams a, WithKind a, WithTypeParams a, WithType a)
    => a
    -> EqualitiesGenerator d e (Type, Kind, Sort)
specialiseExpressionSignature signature = do
    kind <- specialiseDataTypeSignature signature
    specialiseTypeSignature kind signature