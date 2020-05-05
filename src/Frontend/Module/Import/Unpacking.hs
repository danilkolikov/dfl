{- |
Module      :  Frontend.Module.Import.Unpacking
Description :  Unpacking imported definitions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for unpacking imported definitions into required data structures
-}
module Frontend.Module.Import.Unpacking
    ( unpackImports
    ) where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Frontend.Base
import qualified Frontend.Desugaring.Ast as D
import Frontend.Desugaring.Processor
import Frontend.Inference.BuiltIns
import qualified Frontend.Inference.Class as C
import qualified Frontend.Inference.Kind.Ast as K
import Frontend.Inference.Processor
import Frontend.Inference.Signature
import qualified Frontend.Module.Ast as M
import Frontend.Module.Base
import Frontend.Syntax.Position
import Util.HashMap

-- | Function unpacks collected imports of a module into required data structures
unpackImports :: ModuleImports -> FrontendState
unpackImports imports = builtInState <> selectImports imports

-- | Built in definitions
builtInState :: FrontendState
builtInState =
    FrontendState
        { getFrontendStateDesugaring = builtInImportedGroups
        , getFrontendStateFixity = builtInFixity
        , getFrontendStateInference = builtInInference
        }

builtInImportedGroups :: ImportedGroups
builtInImportedGroups =
    let buildMap = HM.mapWithKey (\k _ -> [withDummyLocation k])
     in ImportedGroups
            { getImportedGroupsTypes = buildMap defaultKindSignatures
            , getImportedGroupsExpressions =
                  buildMap $ defaultConstructors `HM.union` defaultExpressions
            , getImportedGroupsModules = mempty
            }

builtInFixity :: InfixOperators
builtInFixity = HM.empty

builtInInference :: InferenceProcessorOutput
builtInInference =
    InferenceProcessorOutput
        { getInferenceProcessorOutputTypeConstructors = defaultKindSignatures
        , getInferenceProcessorOutputTypeSynonyms = defaultTypeSynonyms
        , getInferenceProcessorOutputClasses = HM.empty
        , getInferenceProcessorOutputDataTypes = HM.empty
        , getInferenceProcessorOutputInstances = HM.empty
        , getInferenceProcessorOutputConstructors = defaultConstructors
        , getInferenceProcessorOutputMethods = HM.empty
        , getInferenceProcessorOutputExpressions =
              HM.map (\sig -> (undefined, sig)) defaultExpressions
        }

-- | Creates a state based on imports
selectImports :: ModuleImports -> FrontendState
selectImports imports
    | ModuleImports { getModuleImportsExplicit = explicit
                    , getModuleImportsImplicit = implicit
                    , getModuleImportsInstances = instances
                    , getModuleImportsNameMapping = nameMapping
                    } <- imports =
        FrontendState
            { getFrontendStateDesugaring = selectImportedGroups nameMapping
            , getFrontendStateFixity = selectFixity explicit
            , getFrontendStateInference =
                  selectInferenceOutput explicit implicit instances
            }

selectImportedGroups :: NameMapping -> ImportedGroups
selectImportedGroups mapping
    | NameMapping { getNameMappingTypes = types
                  , getNameMappingExpressions = expressions
                  , getNameMappingModules = modules
                  } <- mapping =
        let wrapMapping = HM.map (map withDummyLocation . HS.toList)
         in ImportedGroups
                { getImportedGroupsTypes = wrapMapping types
                , getImportedGroupsExpressions = wrapMapping expressions
                , getImportedGroupsModules = HM.keysSet modules
                }

selectFixity :: Explicit -> InfixOperators
selectFixity Explicit {getExplicitExpressions = expressions} =
    HM.mapMaybeWithKey processFixity expressions

processFixity :: Ident -> M.Expression -> Maybe D.FixitySignature
processFixity name expr =
    let wrapFixity M.FixitySignature { M.getFixitySignatureFixity = fixity
                                     , M.getFixitySignaturePrecedence = prec
                                     } =
            D.FixitySignature
                { D.getFixitySignatureName = withDummyLocation name
                , D.getFixitySignatureFixity = fixity
                , D.getFixitySignaturePrecedence = prec
                }
     in wrapFixity <$> getExpressionFixity expr

selectInferenceOutput ::
       Explicit -> Implicit -> Instances -> InferenceProcessorOutput
selectInferenceOutput explicit implicit instances
    | Explicit { getExplicitTypeSynonyms = typeSynonyms
               , getExplicitClasses = classes
               , getExplicitDataTypes = dataTypes
               , getExplicitExpressions = expressions
               } <- explicit
    , Implicit {getImplicitTypeConstructors = implicitTypeConstructors} <-
         implicit =
        let allTypeConstructors =
                mconcat
                    [ HM.map M.getClassSignature classes
                    , HM.map M.getDataTypeSignature dataTypes
                    , HM.map createTypeConstructorSignature typeSynonyms
                    , implicitTypeConstructors
                    ]
            processClass name M.Class { M.getClassContext = context
                                      , M.getClassSignature = signature
                                      , M.getClassDataTypeName = dataTypeName
                                      , M.getClassComponents = components
                                      , M.getClassGetters = getters
                                      , M.getClassDefaultInstanceName = defaultInstance
                                      } =
                C.Class
                    { C.getClassContext = context
                    , C.getClassName = name
                    , C.getClassParam =
                          fst . head $
                          getTypeConstructorSignatureTypeParams signature
                    , C.getClassDataTypeName = dataTypeName
                    , C.getClassGetters = getters
                    , C.getClassMethods = components
                    , C.getClassDefaultInstanceName = defaultInstance
                    }
            processDataType name M.DataType { M.getDataTypeSignature = signature
                                            , M.getDataTypeConstructors = constrs
                                            , M.isNewType = newType
                                            } =
                K.DataType
                    { K.getDataTypeContext =
                          error "Contexts of imported constructors are unknown"
                    , K.getDataTypeName = withDummyLocation name
                    , K.getDataTypeParams =
                          map (withDummyLocation . fst) $
                          getTypeConstructorSignatureTypeParams signature
                    , K.getDataTypeDeriving =
                          error "Deriving of imported constructors are unknown"
                    , K.getDataTypeConstructors =
                          HM.toList $ HM.mapWithKey processConstructor constrs
                    , K.isNewType = newType
                    }
            processConstructor name M.Constructor {M.getConstructorFields = fields} =
                K.Constructor
                    { K.getConstructorName = withDummyLocation name
                    , K.getConstructorArgs =
                          error "Args of imported constructors are unknown"
                    , K.getConstructorFields = fields
                    }
            constructors =
                flatten $
                HM.map
                    (HM.map (M.getExpressionType . M.getConstructorExpression) .
                     M.getDataTypeConstructors)
                    dataTypes
            methods =
                flatten $
                HM.map (HM.map M.getExpressionType . M.getClassMethods) classes
            processExpression expr =
                ( error "Imported expressions don't have a body"
                , M.getExpressionType expr)
         in InferenceProcessorOutput
                { getInferenceProcessorOutputTypeConstructors =
                      allTypeConstructors
                , getInferenceProcessorOutputTypeSynonyms = typeSynonyms
                , getInferenceProcessorOutputClasses =
                      HM.mapWithKey processClass classes
                , getInferenceProcessorOutputDataTypes =
                      HM.mapWithKey processDataType dataTypes
                , getInferenceProcessorOutputInstances = instances
                , getInferenceProcessorOutputConstructors = constructors
                , getInferenceProcessorOutputMethods = methods
                , getInferenceProcessorOutputExpressions =
                      HM.map processExpression expressions
                }
