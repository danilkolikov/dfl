{- |
Module      :  Frontend.Module.Export.Explicit
Description :  Processor for explicit exports
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for processing explicit exports of a module
-}
module Frontend.Module.Export.Explicit
    ( selectExplicitExports
    ) where

import Data.Foldable (asum)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List.NonEmpty (toList)
import Data.Maybe (fromMaybe)

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Signature
import Frontend.Module.Base
import Frontend.Syntax.Position
import Util.HashMap

-- | Selects explicit exports of a module
selectExplicitExports ::
       F.ImpExpList (WithLocation F.Export) -> Explicit -> Explicit
selectExplicitExports exports module'
    | Explicit { getExplicitDataTypes = dataTypes
               , getExplicitTypeSynonyms = typeSynonyms
               , getExplicitClasses = classes
               , getExplicitExpressions = expressions
               } <- module' =
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
    -> Explicit
selectExport dataTypes typeSynonyms classes expressions export =
    case export of
        F.ExportFunction name ->
            mempty
                { getExplicitExpressions =
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
        F.ExportModule name -> processExplicit (getValue name)

processTypeSynonym :: Ident -> TypeSignature -> Explicit
processTypeSynonym name signature =
    mempty {getExplicitTypeSynonyms = HM.singleton name signature}

processDataType ::
       Expressions
    -> Ident
    -> F.ImpExpList (WithLocation Ident)
    -> DataType
    -> Explicit
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
            { getExplicitDataTypes =
                  HM.singleton name $
                  selectConstructorsOfDataType selectedConstructors
            , getExplicitExpressions =
                  intersectKeys selectedConstructors expressions
            }

processClass ::
       DataTypes
    -> Expressions
    -> Ident
    -> F.ImpExpList (WithLocation Ident)
    -> Class
    -> Explicit
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
            { getExplicitClasses =
                  HM.singleton name $ selectMethodsOfClass selectedMethods
            , getExplicitDataTypes = intersectKeys classDataType dataTypes
            , getExplicitExpressions =
                  intersectKeys (selectedMethods <> classDataType) expressions
            }

processExplicit :: Ident -> Explicit
processExplicit _ = mempty -- TODO: support export of modules
