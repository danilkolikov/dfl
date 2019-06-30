{- |
Module      :  Frontend.Inference.Kind.DependencyGroup
Description :  Processing of dependency groups
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for kind inference of dependency groups
-}
module Frontend.Inference.Kind.DependencyGroup where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.Trans.State.Lazy (State, evalState, get, modify)
import Control.Monad.Trans.Writer.Lazy (WriterT, runWriterT, tell)
import Data.Foldable (asum)
import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Kind.Ast
import Frontend.Inference.Kind.Kind
import Frontend.Inference.Kind.ProcessorBase
import qualified Frontend.Syntax.Position as P

-- | Errors which may be encountered during resolution of kinds of a dependency group
data DependencyGroupResolvingError
    = DependencyGroupResolvingErrorUnknownVariable (P.WithLocation Ident) -- ^ Unknown variable
    | DependencyGroupResolvingErrorUnknownType (P.WithLocation Ident) -- ^ Unknown type
    | DependencyGroupResolvingErrorUnknownClass (P.WithLocation Ident) -- ^ Unknown class
    | DependencyGroupResolvingErrorUnusedTypeVariable Ident
                                                      F.TypeSignature -- ^ Class variable isn't used in the type signature
    deriving (Eq, Show)

-- | Local environment of kind resolution
data LocalEnvironment = LocalEnvironment
    { getKindInferenceState :: KindInferenceState -- ^ Types with already resolved kinds
    , getKindMappings :: KindMappings -- ^ Mapping of defined variables to kinds
    , getParamsKindMap :: Maybe IdentToKindMapping -- ^ Mapping of parameters to kinds
    }

-- | Set map of parameters to the environment
setParamsKindMap :: IdentToKindMapping -> LocalEnvironment -> LocalEnvironment
setParamsKindMap m env = env {getParamsKindMap = Just m}

-- | Equalities between kinds
type KindEqualities = [(Kind, Kind)]

-- | Resolver of kinds in a dependency group
type LocalResolver a
     = ReaderT LocalEnvironment (WriterT KindEqualities (ExceptT DependencyGroupResolvingError (State Int))) a

-- | Raise an error
raiseError :: DependencyGroupResolvingError -> LocalResolver a
raiseError = lift . lift . throwE

-- | Generate new ident
generateNewIdent :: LocalResolver Ident
generateNewIdent = do
    counter <-
        lift . lift . lift $ do
            state <- get
            modify $ \c -> c + 1
            return state
    return $ IdentGenerated IdentEnvironmentKindInference counter

-- | Collect equalities between kinds of a single dependency group
resolveDependencyGroup ::
       [Ident]
    -> KindInferenceState
    -> KindMappings
    -> Int
    -> Either DependencyGroupResolvingError (KindInferenceState, KindEqualities)
resolveDependencyGroup group resolverState mappings start =
    let localEnvironment =
            LocalEnvironment
                { getKindInferenceState = resolverState
                , getKindMappings = mappings
                , getParamsKindMap = Nothing
                }
        localResolver = resolveMultiple group
        writer = runReaderT localResolver localEnvironment
        except = runWriterT writer
        state = runExceptT except
     in evalState state start

-- | Collect equalities between kinds of a single dependency group
resolveMultiple :: [Ident] -> LocalResolver KindInferenceState
resolveMultiple names = do
    outputs <- mapM resolveSingleIdent names
    return $ mconcat outputs

-- | Collect equalities between kinds of a single object in a dependency group
resolveSingleIdent :: Ident -> LocalResolver KindInferenceState
resolveSingleIdent name = do
    env <- asks getKindMappings
    let maybeProcessSingle getMap resolve update =
            (\res -> update . HM.singleton name <$> uncurry resolve res) <$>
            HM.lookup name (getMap env)
        resolved =
            asum
                [ maybeProcessSingle
                      getTypeSynonymKindMappings
                      resolveTypeSynonym
                      (\m -> mempty {getResolvedTypeSynonyms = m})
                , maybeProcessSingle
                      getDataTypeKindMappings
                      resolveDataType
                      (\m -> mempty {getResolvedDataTypes = m})
                , maybeProcessSingle
                      getClassKindMappings
                      resolveClass
                      (\m -> mempty {getResolvedClasses = m})
                ]
    fromMaybe (error "Unexpected identifier") resolved

-- | Collect equalities between kinds in a type synonym
resolveTypeSynonym ::
       F.TypeSynonym -> TypeSynonymKindMapping -> LocalResolver TypeSynonym
resolveTypeSynonym F.TypeSynonym { F.getTypeSynonymParams = params
                                 , F.getTypeSynonymType = type'
                                 }
                    --
                    TypeSynonymKindMapping { getTypeSynonymKindMappingName = name
                                           , getTypeSynonymKindMappingParams = mapping
                                           } = do
    let withParams = local (setParamsKindMap mapping)
    resolvedType <- withParams $ resolveType type'
    resolvedParams <- withParams $ mapM resolveVariable params
    return
        TypeSynonym
            { getTypeSynonymName = name
            , getTypeSynonymParams = resolvedParams
            , getTypeSynonymType = resolvedType
            }

-- | Collect equalities between kinds in a data type
resolveDataType :: F.DataType -> DataTypeKindMapping -> LocalResolver DataType
resolveDataType F.DataType { F.getDataTypeParams = params
                           , F.getDataTypeContext = context
                           , F.getDataTypeConstructors = constructors
                           , F.getDataTypeDeriving = deriving'
                           , F.isNewType = newType
                           }
                 --
                 DataTypeKindMapping { getDataTypeKindMappingName = name
                                     , getDataTypeKindMappingParams = mapping
                                     } = do
    let withParams = local (setParamsKindMap mapping)
        processConstructor (name', constr) = do
            resolvedConstr <- resolveConstructor constr
            return (name', resolvedConstr)
    resolvedParams <- withParams $ mapM resolveVariable params
    resolvedContext <- withParams $ mapM resolveConstraint context
    resolvedConstructors <- withParams $ mapM processConstructor constructors
    return
        DataType
            { getDataTypeName = name
            , getDataTypeParams = resolvedParams
            , getDataTypeContext = resolvedContext
            , getDataTypeConstructors = resolvedConstructors
            , getDataTypeDeriving = deriving'
            , isNewType = newType
            }

-- | Collect equalities between kinds in a class
resolveClass :: F.Class -> ClassKindMapping -> LocalResolver Class
resolveClass F.Class {F.getClassName = name, F.getClassContext = context}
              --
              ClassKindMapping { getClassKindMappingParam = param
                               , getClassKindMappingMethods = methods
                               } = do
    let mapping = HM.singleton (getValue param) param
        processMethod (name', (method, localMapping)) = do
            resolvedMethod <- resolveMethod method localMapping
            return (name', resolvedMethod)
        withParams = local (setParamsKindMap mapping)
    resolvedContext <- withParams $ mapM resolveSimpleConstraint context
    resolvedMethods <-
        withParams $ HM.fromList <$> mapM processMethod (HM.toList methods)
    return
        Class
            { getClassName = name
            , getClassParam = param
            , getClassContext = resolvedContext
            , getClassMethods = resolvedMethods
            }

-- | Collect equalities between kinds in a type
resolveType :: P.WithLocation F.Type -> LocalResolver (WithKind Type)
resolveType type' = do
    (res, kind) <-
        case P.getValue type' of
            F.TypeVar name -> do
                foundKind <- lookupKindOfVariable name
                return (TypeVar $ setKind name foundKind, foundKind)
            F.TypeConstr name -> do
                foundKind <- lookupKindOfType name
                return (TypeConstr $ setKind name foundKind, foundKind)
            F.TypeFunction from to -> do
                fromResolved <- resolveType from
                toResolved <- resolveType to
            -- Type of (->) is (* -> * -> *)
                lift $
                    tell
                        [ (getKind fromResolved, KindStar)
                        , (getKind toResolved, KindStar)
                        ]
                return (TypeFunction fromResolved toResolved, KindStar)
            F.TypeApplication func args -> do
                funcResolved <- resolveType func
                argsResolved <- mapM resolveType args
                newIdent <- generateNewIdent
                let resultKind = KindVar newIdent
                    expectedKind =
                        foldr (KindFunction . getKind) resultKind argsResolved
                lift $ tell [(getKind funcResolved, expectedKind)]
                return (TypeApplication funcResolved argsResolved, resultKind)
    return $ setKind (type' $> res) kind

-- | Collect equalities between kinds in a constraint
resolveConstraint ::
       P.WithLocation F.Constraint -> LocalResolver (P.WithLocation Constraint)
resolveConstraint constr =
    (constr $>) <$>
    case P.getValue constr of
        F.ConstraintParam class' param -> do
            classParam <- lookupClassVariable class'
            paramKind <- lookupKindOfVariable param
            lift $ tell [(getKind classParam, paramKind)]
            return $ ConstraintParam class' classParam
        F.ConstraintType class' type' params -> do
            classParamKind <- lookupKindOfClassVariable class'
            foundType <- lookupType type'
            paramsKinds <- mapM resolveType params
            let typeKind = getKind foundType
                expectedKind =
                    foldr (KindFunction . getKind) classParamKind paramsKinds
            lift $ tell [(typeKind, expectedKind)]
            return $ ConstraintType class' foundType paramsKinds

-- | Collect equalities between kinds in a simple constraint
resolveSimpleConstraint ::
       P.WithLocation F.SimpleConstraint
    -> LocalResolver (P.WithLocation SimpleConstraint)
resolveSimpleConstraint constr =
    (constr $>) <$>
    case P.getValue constr of
        F.SimpleConstraint class' param -> do
            classParamKind <- lookupKindOfClassVariable class'
            paramKind <- lookupKindOfVariable param
            lift $ tell [(classParamKind, paramKind)]
            return $ SimpleConstraint class' (setKind param paramKind)

-- | Collect equalities between kinds in a constructor
resolveConstructor :: F.Constructor -> LocalResolver Constructor
resolveConstructor F.Constructor { F.getConstructorName = name
                                 , F.getConstructorArgs = args
                                 , F.getConstructorFields = fields
                                 } = do
    resolvedArgs <- mapM resolveType args
    return
        Constructor
            { getConstructorName = name
            , getConstructorArgs = resolvedArgs
            , getConstructorFields = fields
            }

-- | Collect equalities between kinds in a method
resolveMethod :: F.Method -> IdentToKindMapping -> LocalResolver Method
resolveMethod F.Method { F.getMethodName = name
                       , F.getMethodType = type'
                       , F.getMethodDefault = defaultImp
                       } freeTypeVarMapping = do
    resolvedType <- resolveTypeSignature type' freeTypeVarMapping
    return
        Method
            { getMethodName = name
            , getMethodType = resolvedType
            , getMethodDefault = defaultImp
            }

-- | Collect equalities between kinds in a type signature
resolveTypeSignature ::
       F.TypeSignature -> IdentToKindMapping -> LocalResolver TypeSignature
resolveTypeSignature typeSig@F.TypeSignature { F.getTypeSignatureContext = context
                                             , F.getTypeSignatureType = type'
                                             } freeTypeVarMapping = do
    maybeKindMapping <- asks getParamsKindMap
    let kindMapping = fromMaybe HM.empty maybeKindMapping
        unusedVariables =
            HM.keysSet kindMapping `HS.difference` HM.keysSet freeTypeVarMapping
        unusedVariablesList = HS.toList unusedVariables
    unless (null unusedVariablesList) . raiseError $
        DependencyGroupResolvingErrorUnusedTypeVariable
            (head unusedVariablesList)
            typeSig
    let newEnv = kindMapping `HM.union` freeTypeVarMapping
        withNewEnv env = env {getParamsKindMap = Just newEnv}
        withFreeVars = local withNewEnv
    resolvedContext <- withFreeVars $ mapM resolveConstraint context
    resolvedType <- withFreeVars $ resolveType type'
    return
        TypeSignature
            { getTypeSignatureContext = resolvedContext
            , getTypeSignatureParams = HM.elems freeTypeVarMapping
            , getTypeSignatureType = resolvedType
            }

-- | Resovle a kind for a variable
resolveVariable :: P.WithLocation Ident -> LocalResolver (WithKind Ident)
resolveVariable name = do
    kindMapping <- asks getParamsKindMap
    case kindMapping >>= HM.lookup (P.getValue name) of
        Just res -> return res
        Nothing ->
            raiseError $ DependencyGroupResolvingErrorUnknownVariable name

-- | Get kind of a variable
lookupKindOfVariable :: P.WithLocation Ident -> LocalResolver Kind
lookupKindOfVariable name = getKind <$> resolveVariable name

-- | Resolve a data type
lookupType :: P.WithLocation Ident -> LocalResolver (WithKind Ident)
lookupType name = do
    prepared <- asks getKindMappings
    processed <- asks getKindInferenceState
    let lookupSingle getName m = getName <$> HM.lookup (P.getValue name) m
        foundName =
            asum
                [ lookupSingle
                      (getDataTypeKindMappingName . snd)
                      (getDataTypeKindMappings prepared)
                , lookupSingle
                      (getTypeSynonymKindMappingName . snd)
                      (getTypeSynonymKindMappings prepared)
                , lookupSingle getDataTypeName (getResolvedDataTypes processed)
                , lookupSingle
                      getTypeSynonymName
                      (getResolvedTypeSynonyms processed)
                ]
    case foundName of
        Just type' -> return type'
        Nothing -> raiseError $ DependencyGroupResolvingErrorUnknownType name

-- | Get the kind of a data type
lookupKindOfType :: P.WithLocation Ident -> LocalResolver Kind
lookupKindOfType name = getKind <$> lookupType name

-- | Find class parameter
lookupClassVariable :: P.WithLocation Ident -> LocalResolver (WithKind Ident)
lookupClassVariable name = do
    prepared <- asks getKindMappings
    processed <- asks getKindInferenceState
    let lookupSingle getParam m = getParam <$> HM.lookup (P.getValue name) m
        foundParam =
            asum
                [ lookupSingle
                      (getClassKindMappingParam . snd)
                      (getClassKindMappings prepared)
                , lookupSingle getClassParam (getResolvedClasses processed)
                ]
    case foundParam of
        Just var -> return var
        Nothing -> raiseError $ DependencyGroupResolvingErrorUnknownClass name

-- | Get the kind of a class variable
lookupKindOfClassVariable :: P.WithLocation Ident -> LocalResolver Kind
lookupKindOfClassVariable name = getKind <$> lookupClassVariable name
