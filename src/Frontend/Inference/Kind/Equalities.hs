{- |
Module      :  Frontend.Inference.Kind.Equalities
Description :  Function for generation of equalities
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for generation of equalities between kinds and sorts
-}
module Frontend.Inference.Kind.Equalities where

import Control.Monad (unless, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.Trans.Writer.Lazy (WriterT, execWriterT, tell)
import Data.Bifunctor (second)
import Data.Foldable (asum)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Final.Ast
import Frontend.Inference.Kind.ProcessorBase
import Frontend.Inference.Signature hiding
    ( Constraint(..)
    , Type(..)
    , TypeSignature(..)
    )
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

-- | Collect equalities between kinds of a single dependency group
generateEqualitiesForGroup ::
       [DependencyGroupItemWithSignature]
    -> Signatures
    -> VariableGenerator (Either EqualityGenerationError Equalities)
generateEqualitiesForGroup group signatures =
    let localEnvironment =
            LocalEnvironment
                {getSignatures = signatures, getParamsKindMap = HM.empty}
        localResolver = generateMultipleEqualities group
        writer = runReaderT localResolver localEnvironment
        except = execWriterT writer
     in runExceptT except

-- | Collect equalities between kinds of a single dependency group
generateMultipleEqualities ::
       [DependencyGroupItemWithSignature] -> EqualityGenerator ()
generateMultipleEqualities = mapM_ generateEqualities

-- | Generate equalities for a group item
generateEqualities :: DependencyGroupItemWithSignature -> EqualityGenerator ()
generateEqualities (DependencyGroupItemTypeSynonym typeSynonym signature) = do
    resultKind <-
        withTypeParameters signature $
        generateTypeEqualities (getTypeSynonymType typeSynonym)
    writeSignatureEqualities signature resultKind
generateEqualities (DependencyGroupItemDataType dataType signature) = do
    withTypeParameters signature $
        mapM_ generateConstraintEqualities (getDataTypeContext dataType)
    withTypeParameters signature $
        mapM_
            (generateConstructorEqualities . snd)
            (getDataTypeConstructors dataType)
    writeSignatureEqualities signature KindStar
generateEqualities (DependencyGroupItemClass cls signature) = do
    withTypeParameters signature $
        mapM_ generateSimpleConstraintEqualities (getClassContext cls)
    withTypeParameters signature $
        mapM_
            (generateMethodEqualities . snd)
            (HM.toList . getClassMethods $ cls)
    writeSignatureEqualities signature KindStar

-- | Save equalities, derivable from a type constructor signature
writeSignatureEqualities ::
       TypeConstructorSignature -> Kind -> EqualityGenerator ()
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
       TypeConstructorSignature -> EqualityGenerator a -> EqualityGenerator a
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

-- | Collect equalities between kinds in a constraint
generateConstraintEqualities :: WithLocation Constraint -> EqualityGenerator ()
generateConstraintEqualities constr =
    case getValue constr of
        ConstraintParam class' param -> do
            kind <- lookupTypeVariable param
            classKind <-
                lookupClassSignature class' >>= specialiseClassSignature
            writeKindEqualities [(kind, classKind)]
        ConstraintType class' type' params -> do
            classKind <-
                lookupClassSignature class' >>= specialiseClassSignature
            typeKind <- lookupTypeSignature type' >>= specialiseTypeSignature
            paramsKinds <- mapM generateTypeEqualities params
            let expectedKind = foldr KindFunction classKind paramsKinds
            writeKindEqualities [(typeKind, expectedKind)]

-- | Collect equalities between kinds in a simple constraint
generateSimpleConstraintEqualities ::
       WithLocation SimpleConstraint -> EqualityGenerator ()
generateSimpleConstraintEqualities constr =
    case getValue constr of
        SimpleConstraint class' param -> do
            kind <- lookupTypeVariable param
            classKind <-
                lookupClassSignature class' >>= specialiseClassSignature
            writeKindEqualities [(kind, classKind)]

-- | Collect equalities between kinds in a constructor
generateConstructorEqualities :: Constructor -> EqualityGenerator ()
generateConstructorEqualities Constructor {getConstructorArgs = args} = do
    argKinds <- mapM generateTypeEqualities args
    -- Kind of every argument is *
    writeKindEqualities $ map (\k -> (k, KindStar)) argKinds

-- | Collect equalities between kinds in a method
generateMethodEqualities :: Method -> EqualityGenerator ()
generateMethodEqualities Method {getMethodType = type'} =
    generateTypeSignatureEqualities type'

-- | Collect equalities between kinds in a type signature
generateTypeSignatureEqualities :: TypeSignature -> EqualityGenerator ()
generateTypeSignatureEqualities typeSig@TypeSignature { getTypeSignatureContext = context
                                                      , getTypeSignatureType = type'
                                                      } = do
    paramsMap <- asks getParamsKindMap
    let freeVariables = getTypeVariables type'
        definedVariables = HM.keysSet paramsMap
        unusedVariables = definedVariables `HS.difference` freeVariables
        newVariables = freeVariables `HS.difference` definedVariables
        unusedVariablesList = HS.toList unusedVariables
    unless (null unusedVariablesList) . raiseError $
        EqualityGenerationErrorUnusedTypeVariable
            (head unusedVariablesList)
            typeSig
    newParams <- mapM specialiseIdent $ HS.toList newVariables
    writeHasSortEqualities $ map snd newParams
    let newParamsMap = map (second fst) newParams
        withFreeVars = local (defineParameters newParamsMap)
    withFreeVars $ mapM_ generateConstraintEqualities context
    void . withFreeVars $ generateTypeEqualities type'

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
