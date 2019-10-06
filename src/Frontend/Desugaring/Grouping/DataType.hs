{- |
Module      :  Frontend.Desugaring.Grouping.DataType
Description :  Grouping of data types
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Grouping of data types
-}
module Frontend.Desugaring.Grouping.DataType
    ( groupDataTypes
    , groupDataType
    ) where

import qualified Data.HashMap.Lazy as HM
import Data.List (union)

import Frontend.Desugaring.Grouping.Ast
import Frontend.Desugaring.Grouping.Base
import Frontend.Desugaring.Grouping.Util
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (WithLocation(..))

-- | Find newtype-s among the list of top declarations and group them
groupDataTypes :: [WithLocation I.TopDecl] -> GroupingProcessor DataTypes
groupDataTypes = collectHashMap groupDataType

-- | Desugar a single top declaration to a newtype, or return Nothing
groupDataType :: I.TopDecl -> GroupingProcessor (Maybe (Ident, DataType))
groupDataType (I.TopDeclData context simpleType constrs deriving') =
    let (I.SimpleType name args) = getValue simpleType
        constraints = map wrapConstraint context
        getConstructor c =
            case getValue c of
                (I.ConstrSimple name' types) ->
                    let wrappedName = wrapIdent name'
                        wrappedTypes = map wrapType types
                     in ( getValue wrappedName
                        , Constructor wrappedName wrappedTypes HM.empty)
                (I.ConstrRecord name' fields') ->
                    let fieldPairs =
                            map
                                ((\(I.FieldDecl fieldName type') ->
                                      (getValue (wrapIdent fieldName), type')) .
                                 getValue)
                                fields'
                        types = map snd fieldPairs
                        fieldsMap =
                            HM.fromList (zip (map fst fieldPairs) [0 ..])
                        wrappedName = wrapIdent name'
                        wrappedTypes = map wrapType types
                     in ( getValue wrappedName
                        , Constructor wrappedName wrappedTypes fieldsMap)
        constructors = map getConstructor constrs
        getFields c =
            case getValue c of
                I.ConstrRecord _ fields' ->
                    map
                        ((\(I.FieldDecl name' _) -> wrapIdent name') . getValue)
                        fields'
                _ -> []
        fields =
            case constrs of
                [] -> []
                [c] -> getFields c
                cs -> foldr1 union $ map getFields cs
        getConstructorName' c =
            wrapIdent $
            case getValue c of
                (I.ConstrSimple name' _) -> name'
                (I.ConstrRecord name' _) -> name'
        constructorNames = map getConstructorName' constrs
        dataTypeName = wrapIdent name
        dataTypeArgs = map wrapIdent args
        wrappedDeriving = map wrapIdent deriving'
        dataType =
            DataType
                constraints
                dataTypeName
                dataTypeArgs
                wrappedDeriving
                constructors
                False
     in defineDataType dataTypeName dataType constructorNames fields
groupDataType (I.TopDeclNewType context simpleType constr deriving') =
    let (I.SimpleType name args) = getValue simpleType
        constraints = map wrapConstraint context
        constructors =
            [ case getValue constr of
                  I.NewConstrSimple name' type' ->
                      let wrappedName = wrapIdent name'
                          wrappedType = wrapType type'
                       in ( getValue wrappedName
                          , Constructor wrappedName [wrappedType] HM.empty)
                  I.NewConstrRecord name' getter type' ->
                      let wrappedName = wrapIdent name'
                          wrappedGetter = wrapIdent getter
                          wrappedType = wrapType type'
                       in ( getValue wrappedName
                          , Constructor
                                wrappedName
                                [wrappedType]
                                (HM.singleton (getValue wrappedGetter) 0))
            ]
        fields =
            case getValue constr of
                I.NewConstrRecord _ name' _ -> [wrapIdent name']
                _ -> []
        constructorNames =
            [ wrapIdent $
              case getValue constr of
                  I.NewConstrSimple name' _ -> name'
                  I.NewConstrRecord name' _ _ -> name'
            ]
        dataTypeName = wrapIdent name
        dataTypeArgs = map wrapIdent args
        wrappedDeriving = map wrapIdent deriving'
        dataType =
            DataType
                constraints
                dataTypeName
                dataTypeArgs
                wrappedDeriving
                constructors
                True
     in defineDataType dataTypeName dataType constructorNames fields
groupDataType _ = return Nothing

defineDataType ::
       WithLocation Ident
    -> DataType
    -> [WithLocation Ident]
    -> [WithLocation Ident]
    -> GroupingProcessor (Maybe (Ident, DataType))
defineDataType name dataType constructors fields = do
    defineTypeName name
    mapM_ defineExpressionName constructors
    mapM_ defineExpressionName fields
    return $ Just (getValue name, dataType)
