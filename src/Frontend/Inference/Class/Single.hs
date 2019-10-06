{- |
Module      :  Frontend.Inference.Class.Single
Description :  Functions for processing single classes.
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Function for processing single classes.
-}
module Frontend.Inference.Class.Single
    ( processClass
    ) where

import Control.Monad (liftM2)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap, first, second)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, fromMaybe)

import Core.Ident
import Core.PredefinedIdents
import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Class
import Frontend.Inference.Class.Base
import Frontend.Inference.Constraint
import qualified Frontend.Inference.Kind.Ast as K
import Frontend.Inference.Signature
import Frontend.Inference.Substitution
import Frontend.Inference.Variables
import Frontend.Inference.WithVariables
import Frontend.Syntax.Position
import Util.Debug (lookupMapValue)

-- | Processes a single class
processClass ::
       ClassProcessorOutput
    -> K.Class
    -> TypeConstructorSignature
    -> ClassProcessor ClassProcessorOutput
processClass state class' signature =
    evalVariableGeneratorT $ processClass' state class' signature

type SingleClassProcessor = VariableGeneratorT ClassProcessor

processClass' ::
       ClassProcessorOutput
    -> K.Class
    -> TypeConstructorSignature
    -> SingleClassProcessor ClassProcessorOutput
processClass' state K.Class { K.getClassContext = context
                            , K.getClassName = name
                            , K.getClassParam = param
                            , K.getClassMethods = methodsMap
                            } signature = do
    let className = getValue name
        classParam = getValue param
        superClasses = map getValue context
        superClassNames =
            map (getValue . F.getSimpleConstraintClass) superClasses
        (methodNames, methods) =
            unzip . sortBy (\a b -> compare (fst a) (fst b)) $
            HM.toList methodsMap
        fields = HM.fromList $ zip (superClassNames ++ methodNames) [0 ..]
        dataTypeName =
            IdentGenerated $ GeneratedIdentScoped [className, className]
        environment =
            ProcessingEnvironment
                { getProcessingEnvironmentGetFieldPosition =
                      fromJust . (`HM.lookup` fields)
                , getProcessingEnvironmentFieldsSize = length fields
                , getProcessingEnvironmentClassName = className
                , getProcessingEnvironmentParam = classParam
                , getProcessingEnvironmentDataTypeName = dataTypeName
                }
    -- Process class' properties
    results <-
        liftM2
            (++)
            (mapM (processSuperClass environment state) superClasses)
            (mapM (processMethod environment) methods)
    -- Generate a data type and class
    let (dataType, dataTypeSignature) =
            createDataType signature dataTypeName results
        (defaultInstanceName, defaultInstance) =
            createDefaultInstance className classParam results
        (resultClass, getters) =
            createClass
                context
                className
                classParam
                dataType
                methodNames
                defaultInstanceName
                results
        methodSignatures =
            mconcat $ map getProcessingResultMethodSignatures results
    return
        ClassProcessorOutput
            { getClassProcessorOutputClasses =
                  HM.singleton className resultClass
            , getClassProcessorOutputDataTypes =
                  HM.singleton dataTypeName dataType
            , getClassProcessorOutputSignatures =
                  HM.singleton dataTypeName dataTypeSignature
            , getClassProcessorOutputDefaultInstances =
                  HM.singleton defaultInstanceName defaultInstance
            , getClassProcessorOutputMethods = methodSignatures
            , getClassProcessorOutputGetters = getters
            }

data ProcessingEnvironment = ProcessingEnvironment
    { getProcessingEnvironmentGetFieldPosition :: Ident -> Int
    , getProcessingEnvironmentFieldsSize :: Int
    , getProcessingEnvironmentClassName :: Ident
    , getProcessingEnvironmentParam :: Ident
    , getProcessingEnvironmentDataTypeName :: Ident
    }

data ProcessingResult = ProcessingResult
    { getProcessingResultType :: Type
    , getProcessingResultTypeParams :: TypeParams
    , getProcessingResultKindParams :: KindParams
    , getProcessingResultGetters :: HM.HashMap Ident K.Expression
    , getProcessingResultContext :: [Constraint]
    , getProcessingResultDefaultMethods :: HM.HashMap Ident (WithLocation K.Exp)
    , getProcessingResultMethodSignatures :: HM.HashMap Ident TypeSignature
    }

processSuperClass ::
       ProcessingEnvironment
    -> ClassProcessorOutput
    -> F.SimpleConstraint
    -> SingleClassProcessor ProcessingResult
processSuperClass env state constr
    | ProcessingEnvironment {getProcessingEnvironmentParam = classParam} <- env
    , ClassProcessorOutput { getClassProcessorOutputClasses = classes
                           , getClassProcessorOutputSignatures = signatures
                           } <- state
    , F.SimpleConstraint {F.getSimpleConstraintClass = className} <- constr = do
        let constraintClassName = getValue className
        Class { getClassDataTypeName = dataTypeName
              , getClassGetters = superClassGetters
              } <-
            lift $
            lookupMapValue
                (ClassProcessorErrorUnknownClass className)
                constraintClassName
                classes
        TypeConstructorSignature { getTypeConstructorSignatureKindParams = kindParams
                                 , getTypeConstructorSignatureTypeParams = typeParams
                                 } <-
            lift $
            lookupMapValue
                (ClassProcessorErrorUnknownGeneratedDataType dataTypeName)
                dataTypeName
                signatures
        let generatedKindParams = tail kindParams
            generatedTypeParams = tail typeParams
        newKindVars <- mapM (const generateKindIdent) generatedKindParams
        newTypeVars <- mapM (const generateTypeIdent) generatedTypeParams
        let newKindParams =
                map (second snd) $ zip newKindVars generatedKindParams
            kindSubstitution =
                HM.fromList . map (bimap fst KindVar) $
                zip generatedKindParams newKindVars
            newTypeParams =
                map (second $ substitute kindSubstitution . snd) $
                zip newTypeVars generatedTypeParams
            typeArgs = fmap TypeVar (classParam NE.:| newTypeVars)
            type' = TypeApplication (TypeConstr dataTypeName) typeArgs
            (dataTypeGetterName, dataTypeGetter) =
                createGetter env constraintClassName
            superClassFieldGetters =
                HM.mapWithKey
                    (createSuperClassGetter env dataTypeGetterName)
                    superClassGetters
            gettersMap =
                HM.insert
                    constraintClassName
                    dataTypeGetter
                    superClassFieldGetters
        return
            ProcessingResult
                { getProcessingResultType = type'
                , getProcessingResultTypeParams = newTypeParams
                , getProcessingResultKindParams = newKindParams
                , getProcessingResultGetters = gettersMap
                , getProcessingResultContext = []
                , getProcessingResultDefaultMethods = HM.empty
                , getProcessingResultMethodSignatures = HM.empty
                }

processMethod ::
       ProcessingEnvironment
    -> K.Method
    -> SingleClassProcessor ProcessingResult
processMethod env method
    | ProcessingEnvironment { getProcessingEnvironmentClassName = className
                            , getProcessingEnvironmentParam = param
                            } <- env
    , K.Method { K.getMethodName = name
               , K.getMethodType = signature
               , K.getMethodBody = body
               } <- method
    , TypeSignature { getTypeSignatureKindParams = methodKindParams
                    , getTypeSignatureTypeParams = methodTypeParams
                    , getTypeSignatureType = methodType
                    , getTypeSignatureContext = methodContext
                    } <- signature = do
        let unboundTypeVars = filter ((/= param) . fst) methodTypeParams
            usedKindVars =
                HS.unions $ map (getFreeVariables . snd) unboundTypeVars
            unboundKindVars =
                filter (not . (`HS.member` usedKindVars) . fst) methodKindParams
        newKindVars <- mapM (const generateKindIdent) unboundKindVars
        newTypeVars <- mapM (const generateTypeIdent) unboundTypeVars
        let newKindParams = map (second snd) $ zip newKindVars unboundKindVars
            kindSubstitution =
                HM.fromList . map (bimap fst KindVar) $
                zip unboundKindVars newKindVars
            newTypeParams =
                map (second $ substitute kindSubstitution . snd) $
                zip newTypeVars unboundTypeVars
            typeMapping =
                HM.fromList . map (first fst) $ zip unboundTypeVars newTypeVars
            typeSubstitution = HM.map TypeVar typeMapping
            resultType = substitute typeSubstitution methodType
            resultContext = map (substituteVariables typeMapping) methodContext
            fieldName = getValue name
            (_, getter) = createGetter env fieldName
            defaultMethod = fromMaybe undefinedExp body
            finalSignature =
                signature
                -- Append the current class to the context
                    { getTypeSignatureContext =
                          ConstraintVariable className param : methodContext
                    }
            makeMap = HM.singleton fieldName
        return
            ProcessingResult
                { getProcessingResultType = resultType
                , getProcessingResultTypeParams = newTypeParams
                , getProcessingResultKindParams = newKindParams
                , getProcessingResultGetters = makeMap getter
                , getProcessingResultContext = resultContext
                , getProcessingResultDefaultMethods = makeMap defaultMethod
                , getProcessingResultMethodSignatures = makeMap finalSignature
                }

createDataType ::
       TypeConstructorSignature
    -> Ident
    -> [ProcessingResult]
    -> (K.DataType, TypeConstructorSignature)
createDataType signature dataTypeName' results =
    let argTypes = map getProcessingResultType results
        typeParams =
            getTypeConstructorSignatureTypeParams signature ++
            concatMap getProcessingResultTypeParams results
        kindParams =
            getTypeConstructorSignatureKindParams signature ++
            concatMap getProcessingResultKindParams results
        dataTypeContext = concatMap getProcessingResultContext results
        dataTypeName = withDummyLocation dataTypeName'
        constructor =
            K.Constructor
                { K.getConstructorName = dataTypeName
                , K.getConstructorArgs = argTypes
                , K.getConstructorFields = HM.empty
                }
        dataType =
            K.DataType
                { K.getDataTypeContext = dataTypeContext
                , K.getDataTypeName = dataTypeName
                , K.getDataTypeParams = map (withDummyLocation . fst) typeParams
                , K.getDataTypeDeriving = []
                , K.getDataTypeConstructors = [(dataTypeName', constructor)]
                , K.isNewType = False
                }
        dataTypeSignature =
            signature
                { getTypeConstructorSignatureKindParams = kindParams
                , getTypeConstructorSignatureKind = KindStar
                , getTypeConstructorSignatureTypeParams = typeParams
                }
     in (dataType, dataTypeSignature)

createDefaultInstance ::
       Ident -> Ident -> [ProcessingResult] -> (Ident, DefaultInstance)
createDefaultInstance className classParam results =
    let defaultMethods = mconcat $ map getProcessingResultDefaultMethods results
        defaultInstanceName =
            IdentGenerated $ GeneratedIdentInstance className className
        defaultInstance =
            DefaultInstance
                { getDefaultInstanceClassName = className
                , getDefaultInstanceParam = classParam
                , getDefaultInstanceMethods = defaultMethods
                }
     in (defaultInstanceName, defaultInstance)

createClass ::
       [WithLocation F.SimpleConstraint]
    -> Ident
    -> Ident
    -> K.DataType
    -> [Ident]
    -> Ident
    -> [ProcessingResult]
    -> (Class, HM.HashMap Ident K.Expression)
createClass context className classParam dataType methodNames defaultInstanceName results =
    let dataTypeGetters = mconcat $ map getProcessingResultGetters results
        classGetters = HM.map (getValue . K.getExpressionName) dataTypeGetters
        resultClass =
            Class
                { getClassContext =
                      map removePositionsOfSimpleConstraint context
                , getClassName = className
                , getClassParam = classParam
                , getClassDataTypeName = getValue $ K.getDataTypeName dataType
                , getClassGetters = classGetters
                , getClassMethods = methodNames
                , getClassDefaultInstanceName = defaultInstanceName
                }
     in (resultClass, dataTypeGetters)

createGetter :: ProcessingEnvironment -> Ident -> (Ident, K.Expression)
createGetter ProcessingEnvironment { getProcessingEnvironmentClassName = className
                                   , getProcessingEnvironmentGetFieldPosition = getFieldPosition
                                   , getProcessingEnvironmentFieldsSize = fieldsSize
                                   , getProcessingEnvironmentDataTypeName = dataTypeName
                                   } field =
    let fieldPosition = getFieldPosition field
        -- RecordDesugaring is reused here, because getter is technically a desugared record
        makeIdent =
            withDummyLocation .
            IdentGenerated .
            GeneratedIdent GeneratedIdentEnvironmentRecordDesugaring
        inputIdent = makeIdent (-1)
        failIdent = makeIdent (-2)
        fieldIdent = makeIdent fieldPosition
        caseIdents = map makeIdent [0 .. fieldsSize - 1]
        caseExpr =
            withDummyLocation $
            K.ExpCase
                inputIdent
                (withDummyLocation dataTypeName)
                caseIdents
                (withDummyLocation $ K.ExpVar fieldIdent)
                failIdent
        application =
            withDummyLocation $
            K.ExpApplication
                (withDummyLocation $ K.ExpAbstraction failIdent caseExpr)
                (undefinedExp NE.:| [])
        finalExp = withDummyLocation $ K.ExpAbstraction inputIdent application
        finalIdent = IdentGenerated $ GeneratedIdentScoped [className, field]
        expression =
            K.Expression
                { K.getExpressionName = withDummyLocation finalIdent
                , K.getExpressionBody = finalExp
                , K.getExpressionType = Nothing
                }
     in (finalIdent, expression)

createSuperClassGetter ::
       ProcessingEnvironment -> Ident -> Ident -> Ident -> K.Expression
createSuperClassGetter env classGetterName fieldName fieldGetterName
    | ProcessingEnvironment {getProcessingEnvironmentClassName = className} <-
         env =
        let inputIdent =
                withDummyLocation . IdentGenerated $
                GeneratedIdent GeneratedIdentEnvironmentRecordDesugaring 0
            inputExp = withDummyLocation $ K.ExpVar inputIdent
            fieldGetterExp =
                withDummyLocation . K.ExpVar $ withDummyLocation fieldGetterName
            classGetterExp =
                withDummyLocation . K.ExpVar $ withDummyLocation classGetterName
            innerApplication =
                withDummyLocation $
                K.ExpApplication classGetterExp (inputExp NE.:| [])
            application =
                withDummyLocation $
                K.ExpApplication fieldGetterExp (innerApplication NE.:| [])
            finalExp =
                withDummyLocation $ K.ExpAbstraction inputIdent application
            finalIdent =
                IdentGenerated $ GeneratedIdentScoped [className, fieldName]
            expression =
                K.Expression
                    { K.getExpressionName = withDummyLocation finalIdent
                    , K.getExpressionBody = finalExp
                    , K.getExpressionType = Nothing
                    }
         in expression

undefinedExp :: WithLocation K.Exp
undefinedExp =
    withDummyLocation . K.ExpVar . withDummyLocation $
    IdentUserDefined uNDEFINED
