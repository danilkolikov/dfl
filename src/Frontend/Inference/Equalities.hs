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
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.Trans.Writer.Lazy (WriterT, runWriterT, tell)
import Data.Bifunctor (second)
import qualified Data.HashMap.Lazy as HM

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Signature
import Frontend.Inference.Substitution
import Frontend.Inference.Constraint
import Frontend.Inference.Expression (External(..))
import Frontend.Inference.Variables hiding (Type(..))
import Frontend.Syntax.Position

-- | A system of equalities between types, kinds and sorts
data Equalities = Equalities
    { getTypeEqualities :: [(Type, Type)]
    , getKindEqualities :: [(Kind, Kind)]
    , getSortEqualities :: [(Sort, Sort)]
    , getHasKindEqualities :: [(Type, Kind)]
    , getHasSortEqualities :: [(Kind, Sort)]
    , getTypeConstraints :: [Constraint]
    } deriving (Eq, Show)

instance Semigroup Equalities where
    Equalities t1 k1 s1 hk1 hs1 tc1 <> Equalities t2 k2 s2 hk2 hs2 tc2 =
        Equalities
            (t1 <> t2)
            (k1 <> k2)
            (s1 <> s2)
            (hk1 <> hk2)
            (hs1 <> hs2)
            (tc1 <> tc2)

instance Monoid Equalities where
    mempty = Equalities mempty mempty mempty mempty mempty mempty

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
data EqualitiesGenerationError
    = EqualitiesGenerationErrorUnknownName (WithLocation Ident) -- ^ Unknown name
    | EqualitiesGenerationErrorUnknownType (WithLocation Ident) -- ^ Unknown type
    deriving (Eq, Show)

-- | A generator of equalities in a dependency group
type EqualitiesGenerator
     = ReaderT EqualitiesGeneratorEnvironment (WriterT Equalities (ExceptT EqualitiesGenerationError VariableGenerator))

-- | Runs the equality generator
runEqualitiesGenerator ::
       EqualitiesGenerator a
    -> EqualitiesGeneratorEnvironment
    -> VariableGeneratorState
    -> ( Either EqualitiesGenerationError (a, Equalities)
       , VariableGeneratorState)
runEqualitiesGenerator generator environment varState =
    let variables = runEqualitiesGenerator' generator environment
     in runVariableGenerator variables varState

-- | Runs the equality generator and gets a variable generator
runEqualitiesGenerator' ::
       EqualitiesGenerator a
    -> EqualitiesGeneratorEnvironment
    -> VariableGenerator (Either EqualitiesGenerationError (a, Equalities))
runEqualitiesGenerator' generator environment =
    let writer = runReaderT generator environment
        exceptT = runWriterT writer
     in runExceptT exceptT

-- | Saves equalities between types
writeTypeEqualities :: [(Type, Type)] -> EqualitiesGenerator ()
writeTypeEqualities typeEqualities =
    lift $ tell mempty {getTypeEqualities = typeEqualities}

-- | Saves equalities between kinds
writeKindEqualities :: [(Kind, Kind)] -> EqualitiesGenerator ()
writeKindEqualities kindEqualities =
    lift $ tell mempty {getKindEqualities = kindEqualities}

-- | Saves equalities between sorts
writeSortEqualities :: [(Sort, Sort)] -> EqualitiesGenerator ()
writeSortEqualities sortEqualities =
    lift $ tell mempty {getSortEqualities = sortEqualities}

-- | Saves "sorting" relation between types and kinds
writeHasKindEqualities :: [(Type, Kind)] -> EqualitiesGenerator ()
writeHasKindEqualities equalities =
    lift $ tell mempty {getHasKindEqualities = equalities}

-- | Saves "sorting" relation between kinds and sorts
writeHasSortEqualities :: [(Kind, Sort)] -> EqualitiesGenerator ()
writeHasSortEqualities equalities =
    lift $ tell mempty {getHasSortEqualities = equalities}

-- | Saves type constraints
writeTypeConstraints :: [Constraint] -> EqualitiesGenerator ()
writeTypeConstraints constraints =
    lift $ tell mempty {getTypeConstraints = constraints}

-- | Writes equalities "kind = *" for all provided kinds
writeKindStar :: [Kind] -> EqualitiesGenerator ()
writeKindStar =
    writeKindEqualities . map (\x -> (x, KindStar)) . filter (/= KindStar)

-- | Writes equalities "sort = []" for all provided sorts
writeSortSquare :: [Sort] -> EqualitiesGenerator ()
writeSortSquare =
    writeSortEqualities . map (\x -> (x, SortSquare)) . filter (/= SortSquare)

-- | Raise an error
raiseError :: EqualitiesGenerationError -> EqualitiesGenerator a
raiseError = lift . lift . throwE

-- | Lifts a variable generator
liftGen :: VariableGenerator a -> EqualitiesGenerator a
liftGen = lift . lift . lift

-- | Defines new kind variables
defineNewKindVariables :: [(Kind, Sort)] -> EqualitiesGenerator ()
defineNewKindVariables = writeHasSortEqualities

-- | Defines provided type variables
defineNewTypeVariables :: [(Type, Kind, Sort)] -> EqualitiesGenerator ()
defineNewTypeVariables vars =
    let hasKind = map (\(t, k, _) -> (t, k)) vars
        hasSort = map (\(_, k, s) -> (k, s)) vars
     in writeHasKindEqualities hasKind >> writeHasSortEqualities hasSort

-- | Creates new variables, using provided variable and equalities generators
createNewVariables' ::
       VariableGenerator a
    -> ([a] -> EqualitiesGenerator ())
    -> [Ident]
    -> EqualitiesGenerator [(Ident, a)]
createNewVariables' generate define idents = do
    let createVariables name = (\v -> (name, v)) <$> generate
    vars <- liftGen $ mapM createVariables idents
    define (map snd vars)
    return vars

-- | Creates new kind and sort variables and saves relations between them
createNewKindVariables :: [Ident] -> EqualitiesGenerator [(Ident, (Kind, Sort))]
createNewKindVariables =
    createNewVariables' generateKSVariables defineNewKindVariables

-- | Creates new type, kind and sort variables for idents and writes relations between them
createNewTypeVariables ::
       [Ident] -> EqualitiesGenerator [(Ident, (Type, Kind, Sort))]
createNewTypeVariables =
    createNewVariables' generateTKSVariables defineNewTypeVariables

-- | Creates new variable using provided variable and equalities generators
createNewVariable' ::
       VariableGenerator a
    -> ([a] -> EqualitiesGenerator ())
    -> EqualitiesGenerator a
createNewVariable' generate define = do
    var <- liftGen generate
    define [var]
    return var

-- | Creates new kind and sort variable and saves relations between them
createNewKindVariable :: EqualitiesGenerator (Kind, Sort)
createNewKindVariable =
    createNewVariable' generateKSVariables defineNewKindVariables

-- | Creates new type, kind and sort variable and saves relations between them
createNewTypeVariable :: EqualitiesGenerator (Type, Kind, Sort)
createNewTypeVariable =
    createNewVariable' generateTKSVariables defineNewTypeVariables

-- | Executes generator with provided type constructors
withTypeConstructors ::
       HM.HashMap Ident TypeConstructorSignature
    -> EqualitiesGenerator a
    -> EqualitiesGenerator a
withTypeConstructors constructors = local (defineTypeConstructors constructors)

-- | Executes generator with provided expressions
withExpressions ::
       HM.HashMap Ident TypeSignature
    -> EqualitiesGenerator a
    -> EqualitiesGenerator a
withExpressions expressions = local (defineExpressions expressions)

-- | Executes the generator with provided type variables
withTypeVariables ::
       [(Ident, (Type, Kind, Sort))]
    -> EqualitiesGenerator a
    -> EqualitiesGenerator a
withTypeVariables params = local (defineTypeVariables params)

-- | Executes the generator with provided kind variables
withKindVariables ::
       [(Ident, (Kind, Sort))] -> EqualitiesGenerator a -> EqualitiesGenerator a
withKindVariables params = local (defineKindVariables params)

-- | Finds kind of a variable
lookupKindVariable :: WithLocation Ident -> EqualitiesGenerator (Kind, Sort)
lookupKindVariable name = do
    kindMapping <- asks getKindVariables
    case HM.lookup (getValue name) kindMapping of
        Just res -> return res
        Nothing -> raiseError $ EqualitiesGenerationErrorUnknownName name

-- | Finds the signature of a data type, type synonym or a class
lookupTypeConstructorSignature ::
       WithLocation Ident -> EqualitiesGenerator TypeConstructorSignature
lookupTypeConstructorSignature name = do
    signatures <- asks getTypeConstructorSignatures
    case HM.lookup (getValue name) signatures of
        Just signature -> return signature
        Nothing -> raiseError $ EqualitiesGenerationErrorUnknownType name

-- | Finds the kind of a type constructor
lookupKindOfType ::
       WithLocation Ident
    -> EqualitiesGenerator ((Kind, Sort), Substitution Kind)
lookupKindOfType =
    lookupTypeConstructorSignature >=> specialiseDataTypeSignature

-- | Finds the type of a variable
lookupTypeVariable ::
       WithLocation Ident -> EqualitiesGenerator (Maybe (Type, Kind, Sort))
lookupTypeVariable name = do
    typeMapping <- asks getTypeVariables
    return $ HM.lookup (getValue name) typeMapping

-- | Finds the type of a variable
lookupTypeOfVariable ::
       WithLocation Ident -> EqualitiesGenerator (Type, Kind, Sort)
lookupTypeOfVariable name = do
    maybeType <- lookupTypeVariable name
    case maybeType of
        Just res -> return res
        Nothing -> raiseError $ EqualitiesGenerationErrorUnknownName name

-- | Finds the signature of an expression
lookupExpressionSignature ::
       WithLocation Ident -> EqualitiesGenerator TypeSignature
lookupExpressionSignature name = do
    signatures <- asks getExpressionSignatures
    case HM.lookup (getValue name) signatures of
        Just signature -> return signature
        Nothing -> raiseError $ EqualitiesGenerationErrorUnknownName name

-- | Looks up type of an external expression
lookupTypeOfExpression ::
       WithLocation Ident -> EqualitiesGenerator ((Type, Kind, Sort), External)
lookupTypeOfExpression name = do
    (res, (typeSub, kindSub)) <-
        lookupExpressionSignature name >>= specialiseExpressionSignature
    return (res, External (getValue name) typeSub kindSub)

-- | Generates new kind and sort variables for an ident
specialiseKindVariable :: Ident -> EqualitiesGenerator (Ident, (Kind, Sort))
specialiseKindVariable name = (\x -> (name, x)) <$> createNewKindVariable

-- | Generates new type and kind variables for an ident
specialiseTypeVariable ::
       Ident -> EqualitiesGenerator (Ident, (Type, Kind, Sort))
specialiseTypeVariable name = (\x -> (name, x)) <$> createNewTypeVariable

-- | Specialises the signature of a kind constructor
specialiseKindConstructorSignature ::
       (WithSort a, WithKindParams a) => a -> EqualitiesGenerator Sort
specialiseKindConstructorSignature = return . getFullSort -- Nothing to specialise here

-- | Specialises the signature of a type constructor
specialiseTypeConstructorSignature ::
       (WithKind a, WithKindParams a, WithTypeParams a)
    => Sort
    -> a
    -> EqualitiesGenerator ((Kind, Sort), Substitution Kind)
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
    return ((resultKind, resSort), kindSubstitution)

-- | Specialises the signature of a type
specialiseTypeSignature ::
       (WithType a, WithTypeParams a, WithContext a)
    => ((Kind, Sort), Substitution Kind)
    -> a
    -> EqualitiesGenerator ( (Type, Kind, Sort)
                           , (Substitution Type, Substitution Kind))
specialiseTypeSignature ((expectedKind, expectedSort), kindSubstitution) sig = do
    specialisedType <- mapM (specialiseTypeVariable . fst) (getTypeParams sig)
    -- Ensure that kinds match
    resKind <- liftGen generateKindVariable
    let resultParamKinds = map ((\(_, k, _) -> k) . snd) specialisedType
        resultKind = foldr KindFunction resKind resultParamKinds
    writeKindEqualities [(resultKind, expectedKind)]
    -- Substitute types with new parameters
    let typeVariables = HM.fromList specialisedType
        typeSubstitution = HM.map (\(t, _, _) -> t) typeVariables
        resultType = substitute typeSubstitution (getType sig)
        -- It's guaranteed that each value is a TypeVar
        variableMapping = HM.map (\(TypeVar var) -> var) typeSubstitution
        resultConstraints =
            map (substituteVariables variableMapping) (getContext sig)
    -- Save constraint information
    writeTypeConstraints resultConstraints
    -- Saves information about the kind
    writeHasKindEqualities [(resultType, resKind)]
    return
        ( (resultType, resKind, expectedSort)
        , (typeSubstitution, kindSubstitution))

-- | Specialises the kind of a data type. Function creates new kind and sort
-- | variables and specialises the kind with them.
specialiseDataTypeSignature ::
       (WithSort a, WithKindParams a, WithKind a, WithTypeParams a)
    => a
    -> EqualitiesGenerator ((Kind, Sort), Substitution Kind)
specialiseDataTypeSignature signature = do
    sort <- specialiseKindConstructorSignature signature
    specialiseTypeConstructorSignature sort signature

-- | Specialises the type of an expression. Function creates new type, kind and sort
-- | variables and specialises the type with them.
specialiseExpressionSignature ::
       ( WithSort a
       , WithKindParams a
       , WithKind a
       , WithTypeParams a
       , WithType a
       , WithContext a
       )
    => a
    -> EqualitiesGenerator ( (Type, Kind, Sort)
                           , (Substitution Type, Substitution Kind))
specialiseExpressionSignature signature = do
    kind <- specialiseDataTypeSignature signature
    specialiseTypeSignature kind signature
