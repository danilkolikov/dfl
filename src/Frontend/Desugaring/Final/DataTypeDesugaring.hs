{- |
Module      :  Frontend.Desugaring.Final.DataTypeDesugaring
Description :  Final desugaring of data types
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of data types
-}
module Frontend.Desugaring.Final.DataTypeDesugaring
    ( desugarDataTypes
    , desugarDataType
    ) where

import Data.List(union)
import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.Processor
import Frontend.Desugaring.Final.Util (desugarConstraint)
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (WithLocation(..))

-- | Find newtype-s among the list of top declarations and desugar them
desugarDataTypes :: [WithLocation I.TopDecl] -> DesugaringProcessor DataTypes
desugarDataTypes = collectHashMap desugarDataType

-- | Desugar a single top declaration to a newtype, or return Nothing
desugarDataType :: I.TopDecl -> DesugaringProcessor (Maybe (Ident, DataType))
desugarDataType (I.TopDeclData context simpleType constrs deriving') =
    let (I.SimpleType name args) = getValue simpleType
        constraints = map desugarConstraint context
        getConstructor c =
            case getValue c of
                (I.ConstrSimple name' types) ->
                    (getValue name', Constructor name' types HM.empty)
                (I.ConstrRecord name' fields') ->
                    let fieldPairs =
                            map
                                ((\(I.FieldDecl fieldName type') ->
                                      (getValue fieldName, type')) .
                                 getValue)
                                fields'
                        types = map snd fieldPairs
                        fieldsMap =
                            HM.fromList (zip (map fst fieldPairs) [0 ..])
                     in (getValue name', Constructor name' types fieldsMap)
        constructors = map getConstructor constrs
        getFields c =
            case getValue c of
                I.ConstrRecord _ fields' ->
                    map ((\(I.FieldDecl name' _) -> name') . getValue) fields'
                _ -> []
        fields = case constrs of
            [] -> []
            [c] -> getFields c
            cs -> foldr1 union $ map getFields cs
        getConstructorName' c =
            case getValue c of
                (I.ConstrSimple name' _) -> name'
                (I.ConstrRecord name' _) -> name'
        constructorNames = map getConstructorName' constrs
        dataType = DataType constraints name args deriving' constructors False
     in defineDataType name dataType constructorNames fields
desugarDataType (I.TopDeclNewType context simpleType constr deriving') =
    let (I.SimpleType name args) = getValue simpleType
        constraints = map desugarConstraint context
        constructors =
            [ case getValue constr of
                  I.NewConstrSimple name' type' ->
                      (getValue name', Constructor name' [type'] HM.empty)
                  I.NewConstrRecord name' getter type' ->
                      ( getValue name'
                      , Constructor
                            name'
                            [type']
                            (HM.singleton (getValue getter) 0))
            ]
        fields =
            case getValue constr of
                I.NewConstrRecord _ name' _ -> [name']
                _ -> []
        constructorNames =
            [ case getValue constr of
                  I.NewConstrSimple name' _ -> name'
                  I.NewConstrRecord name' _ _ -> name'
            ]
        dataType = DataType constraints name args deriving' constructors True
     in defineDataType name dataType constructorNames fields
desugarDataType _ = return Nothing

defineDataType ::
       WithLocation Ident
    -> DataType
    -> [WithLocation Ident]
    -> [WithLocation Ident]
    -> DesugaringProcessor (Maybe (Ident, DataType))
defineDataType name dataType constructors fields = do
    defineTypeName name
    mapM_ (`defineDataTypeConstructor` dataType) constructors
    mapM_ (`defineDataTypeField` dataType) fields
    return $ Just (getValue name, dataType)
