{- |
Module      :  Frontend.Desugaring.Record.Base
Description :  Base functions for record desugaring
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base functions for record desugaring
-}
module Frontend.Desugaring.Record.Base where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Record.Ast
import Frontend.Syntax.Position (WithLocation(..))

-- | Errors which may happen during desugaring of records
data RecordDesugaringError
    = RecordDesugaringErrorUnknownField (WithLocation Ident) -- ^ Unknown field
    | RecordDesugaringErrorUnknownConstructor (WithLocation Ident) -- ^ Unknown constructor
    | RecordDesugaringErrorDuplicateField (WithLocation Ident)
                                          (WithLocation Ident) -- ^ Some field is used twice in a binding
    | RecordDesugaringErrorMissingConstructor [WithLocation Ident] -- ^ No constructors including these fields
    deriving (Show, Eq)

-- | Context of record desugaring
data RecordDesugaringContext = RecordDesugaringContext
    { getFieldToTypeMap :: DataTypes
    , getConstructorToTypeMap :: DataTypes
    }

-- | Processor of computations which can throw RecordDesugaringError
type RecordDesugaringExcept = Except RecordDesugaringError

-- | Run record desugaring except
runRecordDesugaringExcept ::
       RecordDesugaringExcept a -> Either RecordDesugaringError a
runRecordDesugaringExcept = runExcept

-- | Raises a RecordDEsugaring error
raiseRDError :: RecordDesugaringError -> RecordDesugaringExcept a
raiseRDError = throwE

-- | Processor of record desugaring
type RecordDesugaringProcessor
     = ReaderT RecordDesugaringContext RecordDesugaringExcept

-- | Run a record desugaring processor
runRecordDesugaringProcessor ::
       RecordDesugaringProcessor a
    -> DataTypes
    -> DataTypes
    -> Either RecordDesugaringError a
runRecordDesugaringProcessor rdp fieldToType constrToType =
    runRecordDesugaringExcept
        (runReaderT
             rdp
             RecordDesugaringContext
                 { getFieldToTypeMap = fieldToType
                 , getConstructorToTypeMap = constrToType
                 })

-- | Function raises a RecordDesugaringError
raiseError :: RecordDesugaringError -> RecordDesugaringProcessor a
raiseError = lift . raiseRDError

-- | Function finds a data type by a specified field
findDataTypeByField :: WithLocation Ident -> RecordDesugaringProcessor DataType
findDataTypeByField name = do
    context <- ask
    let fields = getFieldToTypeMap context
    case HM.lookup (getValue name) fields of
        Just dataType -> return dataType
        Nothing -> raiseError $ RecordDesugaringErrorUnknownField name

-- | Function finds a data type by a specified constructor
findDataTypeByConstructor ::
       WithLocation Ident -> RecordDesugaringProcessor DataType
findDataTypeByConstructor name = do
    context <- ask
    let constructors = getConstructorToTypeMap context
    case HM.lookup (getValue name) constructors of
        Just dataType -> return dataType
        Nothing -> raiseError $ RecordDesugaringErrorUnknownConstructor name

-- | Find a specific constructor in the data type or raise an error
lookupConstructor ::
       WithLocation Ident -> DataType -> RecordDesugaringExcept Constructor
lookupConstructor name dataType =
    case lookup (getValue name) (getDataTypeConstructors dataType) of
        Just c -> return c
        Nothing -> throwE $ RecordDesugaringErrorUnknownConstructor name
