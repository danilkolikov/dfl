{- |
Module      :  Frontend.Export.Explicit
Description :  Processor for explicit exports
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for processing explicit exports of a module
-}
module Frontend.Export.Explicit
    ( selectExplicitExports
    ) where

import Data.Foldable (asum)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List.NonEmpty (toList)
import Data.Maybe (fromMaybe)

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Export.Ast
import Frontend.Inference.Signature
import Frontend.Syntax.Position
import Util.HashMap

-- | Selects explicit exports of a module
selectExplicitExports ::
       F.ImpExpList (WithLocation F.Export) -> Module -> Module
selectExplicitExports exports module'@Module { getModuleDataTypes = dataTypes
                                             , getModuleTypeSynonyms = typeSynonyms
                                             , getModuleClasses = classes
                                             , getModuleInstances = instances
                                             , getModuleExpressions = expressions
                                             } =
    let defaultExport =
            mempty
                { getModuleInstances = instances -- Instances are always exported
                }
     in defaultExport <>
        case exports of
            F.ImpExpAll -> module'
            F.ImpExpNothing -> mempty
            F.ImpExpSome someExports ->
                mconcat . toList $
                fmap
                    (selectExport dataTypes typeSynonyms classes expressions .
                     getValue)
                    someExports

selectExport ::
       DataTypes
    -> Signatures TypeSignature
    -> Classes
    -> Expressions
    -> F.Export
    -> Module
selectExport dataTypes typeSynonyms classes expressions export =
    case export of
        F.ExportFunction name ->
            mempty
                { getModuleExpressions =
                      HM.singleton (getValue name) .
                      lookupOrFail (getValue name) $
                      expressions -- Select only one expression
                }
        F.ExportDataOrClass name components ->
            let name' = getValue name
                typeSynonym =
                    processTypeSynonym name' <$> HM.lookup name' typeSynonyms
                dataType =
                    processDataType expressions name' components <$>
                    HM.lookup name' dataTypes
                class' =
                    processClass dataTypes expressions name' components <$>
                    HM.lookup name' classes
             in fromMaybe mempty $ asum [typeSynonym, dataType, class']
        F.ExportModule name -> processModule (getValue name)

processTypeSynonym :: Ident -> TypeSignature -> Module
processTypeSynonym name signature =
    mempty {getModuleTypeSynonyms = HM.singleton name signature}

processDataType ::
       Expressions
    -> Ident
    -> F.ImpExpList (WithLocation Ident)
    -> DataType
    -> Module
processDataType expressions name components dataType =
    let selectedConstructors =
            case components of
                F.ImpExpAll -> HM.keysSet $ getDataTypeConstructors dataType
                F.ImpExpNothing -> HS.empty
                F.ImpExpSome constructors ->
                    HS.fromList . map getValue $ toList constructors
        selectConstructorsOfDataType names =
            dataType
                { getDataTypeConstructors =
                      intersectKeys names $ getDataTypeConstructors dataType
                }
     in mempty
            { getModuleDataTypes =
                  HM.singleton name $
                  selectConstructorsOfDataType selectedConstructors
            , getModuleExpressions =
                  intersectKeys selectedConstructors expressions
            }

processClass ::
       DataTypes
    -> Expressions
    -> Ident
    -> F.ImpExpList (WithLocation Ident)
    -> Class
    -> Module
processClass dataTypes expressions name components class' =
    let selectedMethods =
            case components of
                F.ImpExpAll -> HM.keysSet $ getClassMethods class'
                F.ImpExpNothing -> HS.empty
                F.ImpExpSome methods ->
                    HS.fromList . map getValue $ toList methods
        selectMethodsOfClass names =
            class'
                { getClassGetters = intersectKeys names $ getClassGetters class'
                , getClassMethods = intersectKeys names $ getClassMethods class'
                }
        classDataType = HS.singleton $ getClassDataTypeName class'
     in mempty
            { getModuleClasses =
                  HM.singleton name $ selectMethodsOfClass selectedMethods
            , getModuleDataTypes = intersectKeys classDataType dataTypes
            , getModuleExpressions =
                  intersectKeys (selectedMethods <> classDataType) expressions
            }

processModule :: Ident -> Module
processModule _ = mempty -- TODO: support export of modules
