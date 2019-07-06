{- |
Module      :  Frontend.Inference.Kind.KindMapping
Description :  Mapping of identifiers to kinds
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for creation of mapping from identifiers to kinds
-}
module Frontend.Inference.Kind.KindMapping where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.State.Lazy (State, evalState, get, modify)
import Data.Foldable (asum)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, fromMaybe)

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Kind.Ast
import Frontend.Inference.Kind.ProcessorBase
import qualified Frontend.Syntax.Position as P

-- | Associate free variable in a dependency group with kinds
createMappingsForIdents :: [Ident] -> Environment -> Int -> KindMappings
createMappingsForIdents names env =
    evalState (runReaderT (createMappingsForIdents' names) env)

-- | Associate free variable in a dependency group with kinds
createMappingsForIdents' ::
       [Ident] -> ReaderT Environment (State Int) KindMappings
createMappingsForIdents' names = do
    outputs <- mapM createMappingsForIdent names
    return $ mconcat outputs

-- | Associate free variable in an object in a dependency group with kinds
createMappingsForIdent :: Ident -> ReaderT Environment (State Int) KindMappings
createMappingsForIdent name = do
    env <- ask
    let maybeProcessSingle getMap makeMapping setResult =
            (\res ->
                 setResult . HM.singleton name . (\t -> (res, t)) <$>
                 lift (makeMapping res)) <$>
            HM.lookup name (getMap env)
        prepared =
            asum
                [ maybeProcessSingle
                      getTypeSynonyms
                      makeTypeSynonymKindMapping
                      (\res -> mempty {getTypeSynonymKindMappings = res})
                , maybeProcessSingle
                      getDataTypes
                      makeDataTypeKindMapping
                      (\res -> mempty {getDataTypeKindMappings = res})
                , maybeProcessSingle
                      getClasses
                      makeClassKindMapping
                      (\res -> mempty {getClassKindMappings = res})
                ]
    -- This error should not occur, because we expect that all idents are
    -- either type synonyms, data types or classes
    fromMaybe (error $ "Unexpected identifier " ++ show name) prepared

-- | Set kinds to free variable of a type synonym
makeTypeSynonymKindMapping :: F.TypeSynonym -> State Int TypeSynonymKindMapping
makeTypeSynonymKindMapping F.TypeSynonym { F.getTypeSynonymName = name
                                         , F.getTypeSynonymParams = params
                                         } = do
    kindIdent <- generateNewIdent
    newParams <- mapM addKindVariable params
    let kindVar = KindVar kindIdent
        resultKind = foldr (KindFunction . getKind) kindVar newParams
        newName = setKind name resultKind
    return
        TypeSynonymKindMapping
            { getTypeSynonymKindMappingName = newName
            , getTypeSynonymKindMappingParams = createMapping newParams
            }

-- | Set kinds to free variable of a data type
makeDataTypeKindMapping :: F.DataType -> State Int DataTypeKindMapping
makeDataTypeKindMapping F.DataType { F.getDataTypeName = name
                                   , F.getDataTypeParams = params
                                   } = do
    newParams <- mapM addKindVariable params
    let resultKind = foldr (KindFunction . getKind) KindStar newParams
        newName = setKind name resultKind
    return
        DataTypeKindMapping
            { getDataTypeKindMappingName = newName
            , getDataTypeKindMappingParams = createMapping newParams
            }

-- | Set kinds to free variable of a class
makeClassKindMapping :: F.Class -> State Int ClassKindMapping
makeClassKindMapping F.Class { F.getClassParam = param
                             , F.getClassMethods = methods
                             } = do
    newParam <- addKindVariable param
    let makeMethod (name, method) = do
            methodMapping <-
                makeTypeSignatureKindMapping
                    (P.getValue param)
                    (F.getMethodType method)
            return (name, (method, methodMapping))
    newMethods <- HM.fromList <$> mapM makeMethod (HM.toList methods)
    return
        ClassKindMapping
            { getClassKindMappingParam = newParam
            , getClassKindMappingMethods = newMethods
            }

-- | Set kinds to free variable of a type signature
makeTypeSignatureKindMapping ::
       Ident -> F.TypeSignature -> State Int IdentToKindMapping
makeTypeSignatureKindMapping classParam F.TypeSignature {F.getTypeSignatureType = type'} = do
    let typeVariables = getTypeVariables type'
        getTypeVar = fromJust . (`HM.lookup` typeVariables)
        typeVariablesSet = HS.fromList $ HM.keys typeVariables
        freeTypeVariables =
            typeVariablesSet `HS.difference` HS.singleton classParam
        freeTypeVariablesWL = map getTypeVar $ HS.toList freeTypeVariables
    freeVariablesWithKind <- mapM addKindVariable freeTypeVariablesWL
    return $ createMapping freeVariablesWithKind

-- | Get variables in a type
getTypeVariables ::
       P.WithLocation F.Type -> HM.HashMap Ident (P.WithLocation Ident)
getTypeVariables aType =
    case P.getValue aType of
        F.TypeVar wl -> HM.singleton (P.getValue wl) wl
        F.TypeConstr _ -> HM.empty
        F.TypeFunction from to ->
            getTypeVariables from `HM.union` getTypeVariables to
        F.TypeApplication func args ->
            HM.unions . map getTypeVariables $ func : NE.toList args

-- | Create new ident
generateNewIdent :: State Int Ident
generateNewIdent = do
    counter <- get
    modify $ \c -> c + 1
    return $ IdentGenerated IdentEnvironmentDependencyGroupResolution counter

-- | Add kind variable to an object
addKindVariable :: P.WithLocation a -> State Int (WithKind a)
addKindVariable x = setKind x . KindVar <$> generateNewIdent
