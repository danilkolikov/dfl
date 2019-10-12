{- |
Module      :  Frontend.Export.Implicit
Description :  Processor for implicit exports
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for processing implicit exports of a module
-}
module Frontend.Export.Implicit
    ( ImplicitExport(..)
    , emptyImplicitExport
    , selectImplicitExports
    ) where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List.NonEmpty (NonEmpty, toList)

import Frontend.Export.Ast
import Frontend.Inference.Constraint
import Frontend.Inference.Signature
import Util.HashMap

-- | Implicit exports of a module
newtype ImplicitExport = ImplicitExport
    { getImplicitExportTypeConstructors :: Signatures TypeConstructorSignature
    } deriving (Eq, Show)

-- | Empty implicit exports
emptyImplicitExport :: ImplicitExport
emptyImplicitExport = ImplicitExport mempty

-- | Selects implicit exports of a module
selectImplicitExports ::
       Module -> Signatures TypeConstructorSignature -> ImplicitExport
selectImplicitExports explicit signatures =
    let requiredTypes = getRequiredTypes explicit
     in ImplicitExport
            { getImplicitExportTypeConstructors =
                  intersectKeys requiredTypes signatures
            }

class RequiresTypes a where
    getRequiredTypes :: a -> HS.HashSet Ident

instance RequiresTypes Module where
    getRequiredTypes Module { getModuleDataTypes = dataTypes
                            , getModuleTypeSynonyms = typeSynonyms
                            , getModuleClasses = classes
                            , getModuleInstances = instances
                            , getModuleExpressions = expressions
                            } =
        mconcat
            [ getRequiredTypes dataTypes
            , getRequiredTypes typeSynonyms
            , getRequiredTypes classes
            , getRequiredTypes instances
            , getRequiredTypes expressions
            ]

instance (RequiresTypes v) => RequiresTypes (HM.HashMap k v) where
    getRequiredTypes = mconcat . map getRequiredTypes . HM.elems

instance (RequiresTypes a) => RequiresTypes [a] where
    getRequiredTypes = mconcat . map getRequiredTypes

instance (RequiresTypes a) => RequiresTypes (NonEmpty a) where
    getRequiredTypes = getRequiredTypes . toList

instance RequiresTypes DataType where
    getRequiredTypes = getRequiredTypes . getDataTypeConstructors

instance RequiresTypes Constructor where
    getRequiredTypes = getRequiredTypes . getConstructorExpression

instance RequiresTypes Class where
    getRequiredTypes Class { getClassContext = context
                           , getClassMethods = methods
                           } =
        getRequiredTypes context <> getRequiredTypes methods

instance RequiresTypes Instance where
    getRequiredTypes Instance { getInstanceContext = context
                              , getInstanceClass = class'
                              , getInstanceType = type'
                              } =
        getRequiredTypes context <> HS.fromList [class', type']

instance RequiresTypes Expression where
    getRequiredTypes = getRequiredTypes . getExpressionType

instance RequiresTypes TypeSignature where
    getRequiredTypes TypeSignature { getTypeSignatureContext = context
                                   , getTypeSignatureType = type'
                                   } =
        mconcat $ getRequiredTypes type' : map getRequiredTypes context

instance RequiresTypes Type where
    getRequiredTypes type' =
        case type' of
            TypeVar {} -> HS.empty
            TypeConstr name -> HS.singleton name
            TypeFunction from to -> getRequiredTypes from <> getRequiredTypes to
            TypeApplication func args ->
                getRequiredTypes func <> getRequiredTypes args

instance RequiresTypes SimpleConstraint where
    getRequiredTypes (SimpleConstraint class' _) = HS.singleton class'

instance RequiresTypes Constraint where
    getRequiredTypes constraint =
        case constraint of
            ConstraintVariable class' _ -> HS.singleton class'
            ConstraintAppliedVariable class' _ args ->
                HS.singleton class' <> getRequiredTypes args
