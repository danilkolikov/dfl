{- |
Module      :  Frontend.Module.Explicit
Description :  Selection of explicit imports and exports
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for selecting objects to import/export
-}
module Frontend.Module.Explicit where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Foldable (asum)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List.NonEmpty (toList)

import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Module.Base
import Frontend.Syntax.Position
import Util.HashMap

-- | Errors which can be encountered during prococessing of explicit imports
-- and exports
newtype ExplicitProcessorError =
    ExplicitProcessorErrorUnknownName (WithLocation Ident)
    deriving (Eq, Show)

-- | A type of the explicit import/export processor
type ExplicitProcessor = ReaderT Explicit (Except ExplicitProcessorError)

-- | Executes an import processor
runExplicitProcessor ::
       ExplicitProcessor a -> Explicit -> Either ExplicitProcessorError a
runExplicitProcessor processor = runExcept . runReaderT processor

-- | Returns the list of defined modules
getExplicit :: ExplicitProcessor Explicit
getExplicit = ask

-- | Raises an error
raiseError :: ExplicitProcessorError -> ExplicitProcessor a
raiseError = lift . throwE

-- | A class for types which suport selection of subset of explicit
-- imports or exports
class SelectsExplicit a where
    selectExplicit :: a -> ExplicitProcessor Explicit

instance (SelectsExplicit a) => SelectsExplicit (WithLocation a) where
    selectExplicit = selectExplicit . getValue

instance (SelectsExplicit a) => SelectsExplicit (I.ImpExpList a) where
    selectExplicit list =
        case list of
            I.ImpExpNothing -> return mempty
            I.ImpExpAll -> getExplicit
            I.ImpExpSome imports ->
                mconcat . toList <$> mapM selectExplicit imports

-- | Processes an import/export of a single function
processFunction :: WithLocation Ident -> ExplicitProcessor Explicit
processFunction name = do
    selected <-
        lookupExplicit getExplicitExpressions name >>= failIfUnknown name
    return
        mempty {getExplicitExpressions = HM.singleton (getValue name) selected}

-- | Processes an import/export of a data type or a class
processDataOrClass ::
       WithLocation Ident
    -> I.ImpExpList (WithLocation Ident)
    -> ExplicitProcessor Explicit
processDataOrClass name components = do
    typeSynonym <-
        lookupExplicit getExplicitTypeSynonyms name >>=
        traverse (processTypeSynonym name)
    dataType <-
        lookupExplicit getExplicitDataTypes name >>=
        traverse (processDataType name components)
    class' <-
        lookupExplicit getExplicitClasses name >>=
        traverse (processClass name components)
    failIfUnknown name $ asum [typeSynonym, dataType, class']

-- | Processes an import/export of a type synonym
processTypeSynonym ::
       WithLocation Ident -> TypeSignature -> ExplicitProcessor Explicit
processTypeSynonym name signature =
    return
        mempty
            {getExplicitTypeSynonyms = HM.singleton (getValue name) signature}

-- | Processes an import/export of a data type
processDataType ::
       WithLocation Ident
    -> I.ImpExpList (WithLocation Ident)
    -> DataType
    -> ExplicitProcessor Explicit
processDataType name components dataType = do
    let selectedConstructors =
            case components of
                I.ImpExpAll -> HM.keysSet $ getDataTypeConstructors dataType
                I.ImpExpNothing -> HS.empty
                I.ImpExpSome constructors ->
                    HS.fromList . map getValue $ toList constructors
        selectConstructorsOfDataType names =
            dataType
                { getDataTypeConstructors =
                      intersectKeys names $ getDataTypeConstructors dataType
                }
    return
        mempty
            { getExplicitDataTypes =
                  HM.singleton (getValue name) $
                  selectConstructorsOfDataType selectedConstructors
            }

-- | Processes an import/export of a class
processClass ::
       WithLocation Ident
    -> I.ImpExpList (WithLocation Ident)
    -> Class
    -> ExplicitProcessor Explicit
processClass name components class' = do
    Explicit {getExplicitDataTypes = dataTypes} <- getExplicit
    let selectedMethods =
            case components of
                I.ImpExpAll -> HM.keysSet $ getClassMethods class'
                I.ImpExpNothing -> HS.empty
                I.ImpExpSome methods ->
                    HS.fromList . map getValue $ toList methods
        selectMethodsOfClass names =
            class'
                { getClassGetters = intersectKeys names $ getClassGetters class'
                , getClassMethods = intersectKeys names $ getClassMethods class'
                }
        classDataType = HS.singleton $ getClassDataTypeName class'
    return
        mempty
            { getExplicitClasses =
                  HM.singleton (getValue name) $
                  selectMethodsOfClass selectedMethods
            , getExplicitDataTypes = intersectKeys classDataType dataTypes
            }

-- | Looks up an object in some field of explicit imports/exports
lookupExplicit ::
       (Explicit -> HM.HashMap Ident a)
    -> WithLocation Ident
    -> ExplicitProcessor (Maybe a)
lookupExplicit getMap name = HM.lookup (getValue name) . getMap <$> getExplicit

-- | Raises an error if an object with a provided name is not found
failIfUnknown :: WithLocation Ident -> Maybe a -> ExplicitProcessor a
failIfUnknown name maybeObject =
    case maybeObject of
        Nothing -> raiseError $ ExplicitProcessorErrorUnknownName name
        Just a -> return a
