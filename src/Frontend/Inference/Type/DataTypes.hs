{- |
Module      :  Frontend.Inference.Type.DataTypes
Description :  Functions for processing data types
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Function for processing data types.
-}
module Frontend.Inference.Type.DataTypes where

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Final.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Creates signatures for constructors
createConstructorSignatures :: DataType -> HM.HashMap Ident TypeSignature
createConstructorSignatures DataType { getDataTypeName = name
                                     , getDataTypeParams = typeParams
                                     , getDataTypeContext = context
                                     , getDataTypeConstructors = constructors
                                     } =
    let dataType = TypeConstr name
        params = map (withDummyLocation . TypeVar) typeParams
        resultType =
            withDummyLocation $
            case params of
                [] -> dataType
                (f:rest) ->
                    TypeApplication (withDummyLocation dataType) (f NE.:| rest)
     in HM.fromList $
        map (createConstructorSignature context resultType . snd) constructors

-- | Creates a type signature for a constructor
createConstructorSignature ::
       [WithLocation Constraint]
    -> WithLocation Type
    -> Constructor
    -> (Ident, TypeSignature)
createConstructorSignature context resultType Constructor { getConstructorName = name
                                                          , getConstructorArgs = args
                                                          } =
    let constructorName = getValue name
        constructorType =
            foldr
                (\from to -> withDummyLocation $ TypeFunction from to)
                resultType
                args
     in ( constructorName
        , TypeSignature
              { getTypeSignatureContext = context
              , getTypeSignatureType = constructorType
              })
