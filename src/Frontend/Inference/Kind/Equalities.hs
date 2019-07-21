{- |
Module      :  Frontend.Inference.Kind.Equalities
Description :  Function for generation of equalities
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for generation of equalities between kinds and sorts
-}
module Frontend.Inference.Kind.Equalities where

import Control.Monad ((>=>), unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.Trans.Writer.Lazy (WriterT, execWriterT, runWriterT, tell)
import Data.Bifunctor (second)
import Data.Foldable (asum)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Final.Ast
import Frontend.Inference.Kind.ProcessorBase
import Frontend.Inference.Kind.Signatures (createSignature)
import Frontend.Inference.Signature hiding
    ( Constraint(..)
    , Type(..)
    , TypeSignature(..)
    )
import qualified Frontend.Inference.Signature as S
import Frontend.Inference.Substitution
import Frontend.Inference.Variables hiding (Type(..))
import Frontend.Syntax.Position

-- | Errors which may be encountered during resolution of kinds of a dependency group
data EqualityGenerationError
    = EqualityGenerationErrorUnknownVariable (WithLocation Ident) -- ^ Unknown variable
    | EqualityGenerationErrorUnknownType (WithLocation Ident) -- ^ Unknown type
    | EqualityGenerationErrorUnknownClass (WithLocation Ident) -- ^ Unknown class
    | EqualityGenerationErrorUnusedTypeVariable Ident
                                                TypeSignature -- ^ Class variable isn't used in the type signature
    deriving (Eq, Show)

-- | A local environment of kind resolution
data LocalEnvironment = LocalEnvironment
    { getSignatures :: Signatures -- ^ Types with already resolved kinds
    , getParamsKindMap :: HM.HashMap Ident Kind -- ^ Mapping of parameters to kinds
    }

-- | Resolver of kinds in a dependency group
type EqualityGenerator a
     = ReaderT LocalEnvironment (WriterT Equalities (ExceptT EqualityGenerationError VariableGenerator)) a

-- | Define new parameters
defineParameters :: [(Ident, Kind)] -> LocalEnvironment -> LocalEnvironment
defineParameters m env =
    env {getParamsKindMap = getParamsKindMap env `HM.union` HM.fromList m}

-- | Save equalities between kinds
writeKindEqualities :: [(Kind, Kind)] -> EqualityGenerator ()
writeKindEqualities kindEqualities =
    lift $ tell mempty {getKindEqualities = kindEqualities}

-- | Save equalities between sorts
writeSortEqualities :: [(Sort, Sort)] -> EqualityGenerator ()
writeSortEqualities sortEqualities =
    lift $ tell mempty {getSortEqualities = sortEqualities}

-- | Save "sorting" relation between kinds and sorts
writeHasSortEqualities :: [(Kind, Sort)] -> EqualityGenerator ()
writeHasSortEqualities equalities =
    lift $ tell mempty {getHasSortEqualities = equalities}

-- | Raise an error
raiseError :: EqualityGenerationError -> EqualityGenerator a
raiseError = lift . lift . throwE

-- | Lift variable generator
liftGen :: VariableGenerator a -> EqualityGenerator a
liftGen = lift . lift . lift

-- | Collect equalities between kinds of a single object
generateEqualitiesForObject ::
       (WithEqualities a)
    => a
    -> Signatures
    -> VariableGenerator (Either EqualityGenerationError Equalities)
generateEqualitiesForObject object signatures =
    let localEnvironment =
            LocalEnvironment
                {getSignatures = signatures, getParamsKindMap = HM.empty}
        localResolver = generateEqualities object
        writer = runReaderT localResolver localEnvironment
        except = execWriterT writer
     in runExceptT except

-- | Runs the equality generator
runEqualitiesGenerator ::
       EqualityGenerator a
    -> Signatures
    -> Either EqualityGenerationError (a, Equalities)
runEqualitiesGenerator generator signatures =
    let localEnvironment =
            LocalEnvironment
                {getSignatures = signatures, getParamsKindMap = HM.empty}
        writer = runReaderT generator localEnvironment
        except = runWriterT writer
        variables = runExceptT except
     in evalVariableGenerator variables

-- | A class of types for which it's posible to generate equalities between
-- | kinds and sorts
class WithEqualities a where
    generateEqualities :: a -> EqualityGenerator () -- ^ Generate equalities for an object

instance (WithEqualities a) => WithEqualities (WithLocation a) where
    generateEqualities = generateEqualities . getValue

instance (WithEqualities a) => WithEqualities [a] where
    generateEqualities = mapM_ generateEqualities

instance (WithSort s, WithKindParams s, WithKind s, WithTypeParams s) =>
         WithEqualities (DependencyGroupItem s) where
    generateEqualities (DependencyGroupItemTypeSynonym typeSynonym signature) = do
        resultKind <-
            withTypeParameters signature $
            generateTypeEqualities (getTypeSynonymType typeSynonym)
        writeSignatureEqualities signature resultKind
    generateEqualities (DependencyGroupItemDataType dataType signature) = do
        withTypeParameters signature . generateEqualities $
            getDataTypeContext dataType
        withTypeParameters signature . generateEqualities . map snd $
            getDataTypeConstructors dataType
        writeSignatureEqualities signature KindStar
    generateEqualities (DependencyGroupItemClass cls signature) = do
        withTypeParameters signature . generateEqualities $ getClassContext cls
        withTypeParameters signature $
            generateEqualities . map snd . HM.toList . getClassMethods $ cls
        writeSignatureEqualities signature KindStar

-- | Save equalities, derivable from a type constructor signature
writeSignatureEqualities ::
       (WithSort s, WithKindParams s, WithKind s, WithTypeParams s)
    => s
    -> Kind
    -> EqualityGenerator ()
writeSignatureEqualities signature resultKind = do
    let params = getTypeParams signature
    -- Assign sorts to kind variables
    let addSortVariable (_, kind) = do
            sortVariable <- generateSortVariable
            return (kind, sortVariable)
    withVariables <- lift . lift . lift $ mapM addSortVariable params
    writeHasSortEqualities withVariables
    -- Assert that the result kind has an appropriate sort
    let signatureKind = getFullKind signature
        signatureSort = getFullSort signature
    writeHasSortEqualities [(signatureKind, signatureSort)]
    -- Assert that the inferred kind matches the required one
    let inferredKind = foldr (KindFunction . snd) resultKind params
    writeKindEqualities [(inferredKind, signatureKind)]

-- | Execute the action with the list of parameters
withTypeParameters ::
       (WithTypeParams s) => s -> EqualityGenerator a -> EqualityGenerator a
withTypeParameters signature =
    local (defineParameters $ getTypeParams signature)

-- | Collect equalities between kinds in a type
generateTypeEqualities :: WithLocation Type -> EqualityGenerator Kind
generateTypeEqualities type' =
    case getValue type' of
        TypeVar name -> lookupTypeVariable name
        TypeConstr name -> lookupTypeSignature name >>= specialiseTypeSignature
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

instance WithEqualities Constraint where
    generateEqualities constr =
        case constr of
            ConstraintParam class' param -> do
                kind <- lookupTypeVariable param
                classKind <-
                    lookupClassSignature class' >>= specialiseClassSignature
                writeKindEqualities [(kind, classKind)]
            ConstraintType class' type' params -> do
                classKind <-
                    lookupClassSignature class' >>= specialiseClassSignature
                typeKind <-
                    lookupTypeSignature type' >>= specialiseTypeSignature
                paramsKinds <- mapM generateTypeEqualities params
                let expectedKind = foldr KindFunction classKind paramsKinds
                writeKindEqualities [(typeKind, expectedKind)]

instance WithEqualities SimpleConstraint where
    generateEqualities constr =
        case constr of
            SimpleConstraint class' param -> do
                kind <- lookupTypeVariable param
                classKind <-
                    lookupClassSignature class' >>= specialiseClassSignature
                writeKindEqualities [(kind, classKind)]

instance WithEqualities Constructor where
    generateEqualities Constructor {getConstructorArgs = args} = do
        argKinds <- mapM generateTypeEqualities args
    -- Kind of every argument is *
        writeKindEqualities $ map (\k -> (k, KindStar)) argKinds

instance WithEqualities Method where
    generateEqualities = generateEqualities . getMethodType

instance WithEqualities TypeSignature where
    generateEqualities = addParamsToTypeSignature >=> generateEqualities

-- | A type which stores a type signature and new variables, generated for its params
data TypeSignatureWithParams =
    TypeSignatureWithParams TypeSignature
                            S.TypeConstructorSignature

-- | Generates new kinds for type variables of a signature
addParamsToTypeSignature ::
       TypeSignature -> EqualityGenerator TypeSignatureWithParams
addParamsToTypeSignature sig@TypeSignature {getTypeSignatureType = type'} = do
    signature <-
        lift . lift . lift . createSignature . HS.toList . getTypeVariables $
        type'
    return $ TypeSignatureWithParams sig signature

instance WithEqualities TypeSignatureWithParams where
    generateEqualities (TypeSignatureWithParams typeSig params) = do
        paramsMap <- asks getParamsKindMap
        let freeVariables = HS.fromList . map fst . S.getTypeParams $ params
            definedVariables = HM.keysSet paramsMap
            unusedVariables = definedVariables `HS.difference` freeVariables
            unusedVariablesList = HS.toList unusedVariables
        unless (null unusedVariablesList) . raiseError $
            EqualityGenerationErrorUnusedTypeVariable
                (head unusedVariablesList)
                typeSig
        withTypeParameters params . generateEqualities . getTypeSignatureContext $
            typeSig
        resultKind <-
            withTypeParameters params .
            generateTypeEqualities . getTypeSignatureType $
            typeSig
        writeSignatureEqualities params resultKind

-- | Get variables of a type
getTypeVariables :: WithLocation Type -> HS.HashSet Ident
getTypeVariables aType =
    case getValue aType of
        TypeVar wl -> HS.singleton (getValue wl)
        TypeConstr _ -> HS.empty
        TypeFunction from to ->
            getTypeVariables from `HS.union` getTypeVariables to
        TypeApplication func args ->
            HS.unions . map getTypeVariables $ func : NE.toList args

-- | Find kind of a variable
lookupTypeVariable :: WithLocation Ident -> EqualityGenerator Kind
lookupTypeVariable name = do
    kindMapping <- asks getParamsKindMap
    case HM.lookup (getValue name) kindMapping of
        Just res -> return res
        Nothing -> raiseError $ EqualityGenerationErrorUnknownVariable name

-- | Find signature of a data type or a type synonym
lookupTypeSignature ::
       WithLocation Ident -> EqualityGenerator TypeConstructorSignature
lookupTypeSignature name = do
    signatures <- asks getSignatures
    let lookupSingle getter = HM.lookup (getValue name) (getter signatures)
        foundName =
            asum
                [ lookupSingle getDataTypeSignatures
                , lookupSingle getTypeSynonymSignatures
                ]
    case foundName of
        Just type' -> return type'
        Nothing -> raiseError $ EqualityGenerationErrorUnknownType name

-- | Find class signature
lookupClassSignature ::
       WithLocation Ident -> EqualityGenerator TypeConstructorSignature
lookupClassSignature name = do
    signatures <- asks getSignatures
    case HM.lookup (getValue name) (getClassSignatures signatures) of
        Just var -> return var
        Nothing -> raiseError $ EqualityGenerationErrorUnknownClass name

-- | Generate new kind and sort variables for an ident
specialiseIdent :: Ident -> EqualityGenerator (Ident, (Kind, Sort))
specialiseIdent name = do
    kindVar <- liftGen generateKindVariable
    sortVar <- liftGen generateSortVariable
    return (name, (kindVar, sortVar))

-- | Specialise kind of a type signature. Function creates new sort kind and sort
-- | variables and specialises the kind with them.
specialiseTypeSignature :: TypeConstructorSignature -> EqualityGenerator Kind
specialiseTypeSignature sig@TypeConstructorSignature {getTypeConstructorSignatureKindParams = kindParams} = do
    specialised <- mapM (specialiseIdent . fst) kindParams
    -- Save generated mapping from kinds to sorts
    writeHasSortEqualities $ map snd specialised
    -- Ensure that sorts match
    resSort <- liftGen generateSortVariable
    let expectedSort = getFullSort sig
        resultParamSorts = map (snd . snd) specialised
        resultSort = foldr SortFunction resSort resultParamSorts
    writeSortEqualities [(resultSort, expectedSort)]
    -- Substitute kinds with new parameters
    let kindSubstitution = HM.fromList $ map (second fst) specialised
        resultKind = substitute kindSubstitution (getFullKind sig)
    -- Save information about the sort
    writeHasSortEqualities [(resultKind, resultSort)]
    return resultKind

-- | Specialise parameter of a type class
specialiseClassSignature :: TypeConstructorSignature -> EqualityGenerator Kind
specialiseClassSignature sig@TypeConstructorSignature { getTypeConstructorSignatureKindParams = kindParams
                                                      , getTypeConstructorSignatureTypeParams = typeParams
                                                      } = do
    specialised <- mapM (specialiseIdent . fst) kindParams
    -- Save generated mapping from kinds to sorts
    writeHasSortEqualities $ map snd specialised
    -- Ensure that sorts match
    let expectedSort = getFullSort sig
        resultSort = foldr (SortFunction . snd . snd) SortSquare specialised
    writeSortEqualities [(resultSort, expectedSort)]
    -- Substitute kinds with new parameters
    let kindSubstitution = HM.fromList $ map (second fst) specialised
        resultKind = substitute kindSubstitution (getFullKind sig)
    -- Save information about sort
    writeHasSortEqualities [(resultKind, resultSort)]
    let [(_, kind)] = typeParams -- Only 1 parameter is supported now
        paramKind = substitute kindSubstitution kind
    return paramKind
