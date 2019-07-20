{- |
Module      :  Frontend.Inference.Kind.Signatures
Description :  Signatures for dependency groups
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions which create signatures for dependency groups
-}
module Frontend.Inference.Kind.Signatures where

import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Final.Ast hiding (TypeSignature(..))
import Frontend.Inference.Kind.ProcessorBase
import Frontend.Inference.Signature
import Frontend.Inference.Variables
import Frontend.Syntax.Position

-- | Create signatures for a dependency group
createSignaturesForGroup ::
       [DependencyGroupItemEmpty]
    -> VariableGenerator ([DependencyGroupItemWithSignature], Signatures)
createSignaturesForGroup items = do
    outputs <- mapM processItem items
    return $ mconcat outputs

-- | Create a signature for a group item
processItem ::
       DependencyGroupItemEmpty
    -> VariableGenerator ([DependencyGroupItemWithSignature], Signatures)
processItem item =
    let createSingleSignature getName getParams setField wrapResult obj =
            (\x ->
                 ( [wrapResult obj x]
                 , setField (HM.singleton (getValue $ getName obj) x))) <$>
            createSignature (map getValue $ getParams obj)
     in case item of
            DependencyGroupItemTypeSynonym synonym _ ->
                createSingleSignature
                    getTypeSynonymName
                    getTypeSynonymParams
                    (\x -> mempty {getTypeSynonymSignatures = x})
                    DependencyGroupItemTypeSynonym
                    synonym
            DependencyGroupItemDataType dataType _ ->
                createSingleSignature
                    getDataTypeName
                    getDataTypeParams
                    (\x -> mempty {getDataTypeSignatures = x})
                    DependencyGroupItemDataType
                    dataType
            DependencyGroupItemClass cls _ ->
                createSingleSignature
                    getClassName
                    (return . getClassParam)
                    (\x -> mempty {getClassSignatures = x})
                    DependencyGroupItemClass
                    cls

-- | Create a type constructor signatures, using provided type parameters
createSignature :: [Ident] -> VariableGenerator TypeConstructorSignature
createSignature params = do
    let addKindVariable x = do
            kindVariable <- generateKindVariable
            return (x, kindVariable)
    typeParams <- mapM addKindVariable params
    resultKind <- generateKindVariable
    resultSort <- generateSortVariable
    return $
        TypeConstructorSignature
            { getTypeConstructorSignatureSort = resultSort
            , getTypeConstructorSignatureKindParams = []
            , getTypeConstructorSignatureKind = resultKind
            , getTypeConstructorSignatureTypeParams = typeParams
            }
