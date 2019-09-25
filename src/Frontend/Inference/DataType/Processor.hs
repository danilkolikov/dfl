{- |
Module      :  Frontend.Inference.DataType.Processor
Description :  Functions for data type processing
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of data types
-}
module Frontend.Inference.DataType.Processor
    ( DataTypeProcessorError(..)
    , DataTypeProcessorOutput(..)
    , DataTypeProcessorDebugOutput(..)
    , processDataTypes
    ) where

import Data.Bifunctor (second)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE

import qualified Frontend.Desugaring.Final.Ast as F
import qualified Frontend.Inference.Kind.Ast as K
import Frontend.Inference.Signature
import Frontend.Inference.Util.Debug
import Frontend.Inference.WithVariables
import Frontend.Syntax.Position

-- | Type of the error which can be encountered during data type processing
data DataTypeProcessorError
    = DataTypeProcessorErrorUnknownType (WithLocation F.Ident) -- ^ Unknown data type
    | DataTypeProcessorErrorCanNotDerive (WithLocation F.Ident) -- ^ Can't derive instance
    deriving (Eq, Show)

-- | Output of data type processing
data DataTypeProcessorOutput = DataTypeProcessorOutput
    { getDataTypeProcessorOutputConstructors :: HM.HashMap F.Ident TypeSignature
    , getDataTypeProcessorOutputInstances :: [K.Instance]
    } deriving (Eq, Show)

instance Semigroup DataTypeProcessorOutput where
    DataTypeProcessorOutput c1 i1 <> DataTypeProcessorOutput c2 i2 =
        DataTypeProcessorOutput (c1 <> c2) (i1 <> i2)

instance Monoid DataTypeProcessorOutput where
    mempty = DataTypeProcessorOutput mempty mempty

-- | Debug output of data type processing
data DataTypeProcessorDebugOutput = DataTypeProcessorDebugOutput
    { getDataTypeProcessorDebugOutputConstructors :: Maybe (HM.HashMap F.Ident TypeSignature)
    , getDataTypeProcessorDebugOutputInstances :: Maybe [K.Instance]
    } deriving (Eq, Show)

instance Semigroup DataTypeProcessorDebugOutput where
    DataTypeProcessorDebugOutput c1 i1 <> DataTypeProcessorDebugOutput c2 i2 =
        DataTypeProcessorDebugOutput (c1 <> c2) (i1 <> i2)

instance Monoid DataTypeProcessorDebugOutput where
    mempty = DataTypeProcessorDebugOutput mempty mempty

type DataTypeProcessor
     = WithDebugOutput DataTypeProcessorError DataTypeProcessorDebugOutput

-- | Process data types and generate constructors and derived instances
processDataTypes ::
       Signatures TypeConstructorSignature
    -> HM.HashMap F.Ident K.DataType
    -> ( Either DataTypeProcessorError DataTypeProcessorOutput
       , DataTypeProcessorDebugOutput)
processDataTypes signatures dataTypes =
    runWithDebugOutput $ processDataTypes' signatures dataTypes

processDataTypes' ::
       Signatures TypeConstructorSignature
    -> HM.HashMap F.Ident K.DataType
    -> DataTypeProcessor DataTypeProcessorOutput
processDataTypes' signatures =
    (mconcat <$>) . mapM (processDataType signatures) . HM.elems

processDataType ::
       Signatures TypeConstructorSignature
    -> K.DataType
    -> DataTypeProcessor DataTypeProcessorOutput
processDataType signatures K.DataType { K.getDataTypeContext = context
                                      , K.getDataTypeName = name
                                      , K.getDataTypeParams = params
                                      , K.getDataTypeConstructors = constructors
                                      , K.getDataTypeDeriving = deriving'
                                      } = do
    let dataTypeName = getValue name
        dataTypeParams = map getValue params
    dataTypeSignature <-
        lookupMapValue
            (DataTypeProcessorErrorUnknownType name)
            (getValue name)
            signatures
    let processedConstructors =
            HM.fromList $
            map
                (second $
                 processConstructor
                     dataTypeSignature
                     context
                     dataTypeName
                     dataTypeParams)
                constructors
    writeDebugOutput
        mempty
            { getDataTypeProcessorDebugOutputConstructors =
                  Just processedConstructors
            }
    instances <-
        mapM (deriveInstance dataTypeName dataTypeParams constructors) deriving'
    return
        DataTypeProcessorOutput
            { getDataTypeProcessorOutputConstructors = processedConstructors
            , getDataTypeProcessorOutputInstances = instances
            }

processConstructor ::
       TypeConstructorSignature
    -> [Constraint]
    -> F.Ident
    -> [F.Ident]
    -> K.Constructor
    -> TypeSignature
processConstructor signature context typeName typeArgs constructor
    | TypeConstructorSignature { getTypeConstructorSignatureSort = sort
                               , getTypeConstructorSignatureKindParams = kindParams
                               , getTypeConstructorSignatureKind = kind
                               , getTypeConstructorSignatureTypeParams = typeParams
                               } <- signature
    , K.Constructor {K.getConstructorArgs = args} <- constructor =
        let dataTypeType = TypeConstr typeName
            resultType =
                case typeArgs of
                    [] -> dataTypeType
                    (f:rest) ->
                        TypeApplication
                            dataTypeType
                            (fmap TypeVar $ f NE.:| rest)
            finalType = foldr TypeFunction resultType args
            freeTypeVars = getFreeVariables finalType
            newContext =
                filter
                    (any (`HS.member` freeTypeVars) . getFreeVariables)
                    context
            newTypeParams = filter ((`HS.member` freeTypeVars) . fst) typeParams
            freeKindVars =
                HS.unions $ map (getFreeVariables . snd) newTypeParams
            newKindParams = filter ((`HS.member` freeKindVars) . fst) kindParams
         in TypeSignature
                { getTypeSignatureSort = sort
                , getTypeSignatureKindParams = newKindParams
                , getTypeSignatureKind = kind
                , getTypeSignatureTypeParams = newTypeParams
                , getTypeSignatureType = finalType
                , getTypeSignatureContext = newContext
                }

type InstanceGenerator
     = F.Ident -> [F.Ident] -> [(F.Ident, K.Constructor)] -> K.Instance

iNSTANCE_GENERATORS :: HM.HashMap F.Ident InstanceGenerator
iNSTANCE_GENERATORS = HM.empty

deriveInstance ::
       F.Ident
    -> [F.Ident]
    -> [(F.Ident, K.Constructor)]
    -> WithLocation F.Ident
    -> DataTypeProcessor K.Instance
deriveInstance typeName args constructors className = do
    generateInstance <-
        lookupMapValue
            (DataTypeProcessorErrorCanNotDerive className)
            (getValue className)
            iNSTANCE_GENERATORS
    let instance' = generateInstance typeName args constructors
    writeDebugOutput
        mempty {getDataTypeProcessorDebugOutputInstances = Just [instance']}
    return instance'
