{- |
Module      :  Frontend.Desugaring.Record.Expression
Description :  Desugaring of records in expressions and patterns
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of records in expressions and patterns
-}
module Frontend.Desugaring.Record.Processor
    ( desugarModuleRecords
    , RecordDesugaringError(..)
    ) where

import Data.Bifunctor (second)
import qualified Data.HashMap.Lazy as HM

import qualified Frontend.Desugaring.Fixity.Ast as F
import Frontend.Desugaring.Record.Ast
import Frontend.Desugaring.Record.Base
import Frontend.Desugaring.Record.Expression
import Frontend.Desugaring.Record.Field

-- | Desugars records in a module
desugarModuleRecords ::
       Module F.Exp -> Either RecordDesugaringError (Module Exp)
desugarModuleRecords module'
    | Module {getModuleDataTypes = dataTypes} <- module' =
        let fieldToDataType = collectFieldsMap dataTypes
            constructorToDataType = collectConstructorMap dataTypes
         in runRecordDesugaringProcessor
                (resolveModuleRecords module')
                fieldToDataType
                constructorToDataType

resolveModuleRecords :: Module F.Exp -> RecordDesugaringProcessor (Module Exp)
resolveModuleRecords module'
    | Module {getModuleDataTypes = dataTypes} <- module' = do
        resolvedModule <- mapExpressionM desugarExp module'
        getters <- makeFieldGetters dataTypes
        return
            resolvedModule
                { getModuleExpressions =
                      getModuleExpressions resolvedModule <> getters
                }

collectFieldsMap :: DataTypes -> DataTypes
collectFieldsMap = HM.unions . map processDataType . HM.elems
  where
    processDataType d@DataType {getDataTypeConstructors = constructors} =
        HM.unions $ map (processConstructor d . snd) constructors
    processConstructor d Constructor {getConstructorFields = fields} =
        HM.map (const d) fields

collectConstructorMap :: DataTypes -> DataTypes
collectConstructorMap = HM.unions . map processDataType . HM.elems
  where
    processDataType d@DataType {getDataTypeConstructors = constructors} =
        HM.fromList $ map (second $ const d) constructors
