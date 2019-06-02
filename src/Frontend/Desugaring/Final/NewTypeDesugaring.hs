{- |
Module      :  Frontend.Desugaring.Final.NewTypeDesugaring
Description :  Final desugaring of newtypes
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of newtypes
-}
module Frontend.Desugaring.Final.NewTypeDesugaring
    ( desugarNewTypes
    , desugarNewType
    ) where

import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.Processor
import Frontend.Desugaring.Final.Util (desugarConstraint)
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (WithLocation(..))

-- | Find newtype-s among the list of top declarations and desugar them
desugarNewTypes :: [WithLocation I.TopDecl] -> DesugaringProcessor NewTypes
desugarNewTypes = collectHashMap desugarNewType

-- | Desugar a single top declaration to a newtype, or return Nothing
desugarNewType :: I.TopDecl -> DesugaringProcessor (Maybe (Ident, NewType))
desugarNewType (I.TopDeclNewType context simpleType constr deriving') =
    let (I.SimpleType name args) = getValue simpleType
        constraints = map desugarConstraint context
        constructor =
            case getValue constr of
                I.NewConstrSimple name' type' ->
                    (getValue name', Constructor name' [type'] HM.empty)
                I.NewConstrRecord name' getter type' ->
                    ( getValue name'
                    , Constructor
                          name'
                          [type']
                          (HM.singleton (getValue getter) 0))
        field =
            case getValue constr of
                I.NewConstrRecord _ name' _ -> Just name'
                _ -> Nothing
        newType = NewType constraints name args deriving' constructor
     in do defineTypeName name
           mapM_ (`defineNewTypeField` newType) field
           return $ Just (getValue name, newType)
desugarNewType _ = return Nothing
