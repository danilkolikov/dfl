{- |
Module      :  Frontend.Export.Collecting
Description :  Functions for collecting exported expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for collecting exported expressions
-}
module Frontend.Export.Collecting
    ( collectExports
    ) where

import qualified Data.HashMap.Lazy as HM

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Export.Base
import qualified Frontend.Inference.Class as C
import qualified Frontend.Inference.Expression as E
import qualified Frontend.Inference.Kind.Ast as K
import Frontend.Inference.Processor
import Frontend.Inference.Signature
import Util.HashMap

-- | Collects exports of a module
collectExports :: F.Module F.Exp -> InferenceProcessorOutput -> Module
collectExports module' output
    | F.Module { F.getModuleDataTypes = desugaredDataType
               , F.getModuleClasses = desugaredClasses
               , F.getModuleExpressions = desugaredExpressions
               } <- module'
    , InferenceProcessorOutput { getInferenceProcessorOutputTypeConstructors = typeConstructors
                               , getInferenceProcessorOutputTypeSynonyms = typeSynonyms
                               , getInferenceProcessorOutputClasses = classes
                               , getInferenceProcessorOutputDataTypes = dataTypes
                               , getInferenceProcessorOutputInstances = instances
                               , getInferenceProcessorOutputConstructors = constructors
                               , getInferenceProcessorOutputMethods = methods
                               , getInferenceProcessorOutputExpressions = expressions
                               } <- output =
        let collectedClasses =
                collectClasses desugaredClasses typeConstructors classes methods
            collectedDataTypes =
                collectDataTypes desugaredDataType typeConstructors constructors
            collectedGeneratedDataTypes =
                collectGeneratedDataTypes
                    collectedClasses
                    dataTypes
                    typeConstructors
                    constructors
            collectedExpressions =
                collectExpressions desugaredExpressions expressions
         in Module
                { getModuleDataTypes =
                      collectedDataTypes <> collectedGeneratedDataTypes
                , getModuleTypeSynonyms = typeSynonyms
                , getModuleClasses = collectedClasses
                , getModuleInstances = instances
                , getModuleExpressions = collectedExpressions
                }

collectClasses ::
       F.Classes F.Exp
    -> Signatures TypeConstructorSignature
    -> HM.HashMap Ident C.Class
    -> Signatures TypeSignature
    -> Classes
collectClasses desugaredClasses typeConstructors classes methods =
    HM.mapWithKey collectClass desugaredClasses
  where
    collectClass name F.Class {F.getClassMethods = desugaredMethods} =
        let classSignature = lookupOrFail name typeConstructors
            C.Class { C.getClassContext = context
                    , C.getClassDataTypeName = dataTypeName
                    , C.getClassGetters = getters
                    , C.getClassMethods = methodNames
                    , C.getClassDefaultInstanceName = defaultInstanceName
                    } = lookupOrFail name classes
            collectedMethods = map (collectMethod desugaredMethods) methodNames
         in Class
                { getClassContext = context
                , getClassSignature = classSignature
                , getClassDataTypeName = dataTypeName
                , getClassGetters = getters
                , getClassComponents = methodNames
                , getClassMethods = HM.fromList collectedMethods
                , getClassDefaultInstanceName = defaultInstanceName
                }
    collectMethod desugaredMethods name =
        let F.Method {F.getMethodFixity = fixity} =
                lookupOrFail name desugaredMethods
            resultFixity = collectFixity <$> fixity
            signature = lookupOrFail name methods
            expression =
                Expression
                    { getExpressionType = signature
                    , getExpressionFixity = resultFixity
                    }
         in (name, expression)

collectDataTypes ::
       F.DataTypes
    -> Signatures TypeConstructorSignature
    -> Signatures TypeSignature
    -> DataTypes
collectDataTypes desugaredDataType typeConstructors constructors =
    HM.mapWithKey collectDataType desugaredDataType
  where
    collectDataType name F.DataType { F.getDataTypeConstructors = desugaredConstructors
                                    , F.isNewType = newType
                                    } =
        let signature = lookupOrFail name typeConstructors
            collectedConstructors = map collectConstructor desugaredConstructors
         in DataType
                { getDataTypeSignature = signature
                , getDataTypeConstructors = HM.fromList collectedConstructors
                , isNewType = newType
                }
    collectConstructor (name, desugaredConstructor)
        | F.Constructor { F.getConstructorFixity = fixity
                        , F.getConstructorFields = fields
                        } <- desugaredConstructor =
            let signature = lookupOrFail name constructors
                resultFixity = collectFixity <$> fixity
                expression =
                    Expression
                        { getExpressionType = signature
                        , getExpressionFixity = resultFixity
                        }
                constructor =
                    Constructor
                        { getConstructorExpression = expression
                        , getConstructorFields = fields
                        }
             in (name, constructor)

collectGeneratedDataTypes ::
       Classes
    -> HM.HashMap Ident K.DataType
    -> Signatures TypeConstructorSignature
    -> Signatures TypeSignature
    -> DataTypes
collectGeneratedDataTypes classes dataTypes signatures constructors =
    HM.map processClass classes
  where
    processClass Class {getClassDataTypeName = dataTypeName} =
        let signature = lookupOrFail dataTypeName signatures
            K.DataType { K.getDataTypeConstructors = dataTypeConstructors
                       , K.isNewType = newType
                       } = lookupOrFail dataTypeName dataTypes
            collectedConstructors = map collectConstructor dataTypeConstructors
         in DataType
                { getDataTypeSignature = signature
                , getDataTypeConstructors = HM.fromList collectedConstructors
                , isNewType = newType
                }
    collectConstructor (name, K.Constructor {K.getConstructorFields = fields}) =
        let signature = lookupOrFail name constructors
            expression =
                Expression
                    { getExpressionType = signature
                    , getExpressionFixity = Nothing -- Fixity is not defined for generated constructors
                    }
            constructor =
                Constructor
                    { getConstructorExpression = expression
                    , getConstructorFields = fields
                    }
         in (name, constructor)

collectExpressions ::
       F.Expressions F.Exp -> Signatures E.ExpWithSignature -> Expressions
collectExpressions desugaredExpressions expressions =
    HM.mapWithKey collectExpression desugaredExpressions
  where
    collectExpression name F.Expression {F.getExpressionFixity = fixity} =
        let signature = lookupOrFail name expressions
            resultFixity = collectFixity <$> fixity
         in Expression
                { getExpressionType = snd signature
                , getExpressionFixity = resultFixity
                }

-- | Collects fixity
collectFixity :: F.FixitySignature -> FixitySignature
collectFixity F.FixitySignature { F.getFixitySignatureFixity = fixity
                                , F.getFixitySignaturePrecedence = prec
                                } =
    FixitySignature
        {getFixitySignatureFixity = fixity, getFixitySignaturePrecedence = prec}
