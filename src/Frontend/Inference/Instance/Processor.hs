{- |
Module      :  Frontend.Inference.Instance.Processor
Description :  Functions for processing instances.
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Function for processing instances.
-}
module Frontend.Inference.Instance.Processor
    ( InstanceProcessorError(..)
    , InstanceProcessorOutput(..)
    , InstanceProcessorDebugOutput(..)
    , processInstances
    ) where

import Control.Monad (foldM)
import Data.Foldable (asum)
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)

import Frontend.Desugaring.Final.Ast (Ident(..), IdentEnvironment(..))
import qualified Frontend.Inference.Class as C
import Frontend.Inference.Constraint
import qualified Frontend.Inference.Instance as I
import qualified Frontend.Inference.Kind.Ast as K
import Frontend.Inference.Util.Debug
import Frontend.Inference.Util.HashMap
import Frontend.Syntax.Position

-- | A type of errors which can be encountered during instance processing
data InstanceProcessorError
    = InstanceProcessorErrorAlreadyDefined (WithLocation Ident)
                                           (WithLocation Ident) -- ^ Instance is already defined
    | InstanceProcessorErrorUnknownClass Ident -- ^ Unknown class
    | InstanceProcessorErrorUnknownClassComponent Ident
                                                  Ident -- ^ Unknown component of a class (superclass / method)
    | InstanceProcessorErrorUnknownInstance Ident
                                            Ident -- ^ Required instance is not defined
    | InstanceProcessorErrorUnsatisfiedConstraint Ident
                                                  Ident -- ^ Constraint of an instance is not satisfied
    deriving (Eq, Show)

-- | A type of ouput of the instance processor
data InstanceProcessorOutput = InstanceProcessorOutput
    { getInstanceProcessorOutputInstances :: HM.HashMap Ident I.Instance -- ^ A map of defined instances
    , getInstanceProcessorOutputExpressions :: HM.HashMap Ident K.Expression -- ^ A map of generated instance expressions
    } deriving (Eq, Show)

-- | A type of debug output of the instance processor
data InstanceProcessorDebugOutput = InstanceProcessorDebugOutput
    { getInstanceProcessorDebugOutputInstances :: Maybe (HM.HashMap Ident I.Instance) -- ^ A map of defined instances
    , getInstanceProcessorDebugOutputDefaults :: Maybe (HM.HashMap Ident K.Expression) -- ^ A map of expressions for default instances
    , getInstanceProcessorDebugOutputExpressions :: Maybe (HM.HashMap Ident K.Expression) -- ^ A map of expressions for instances
    } deriving (Eq, Show)

instance Semigroup InstanceProcessorDebugOutput where
    InstanceProcessorDebugOutput i1 d1 e1 <> InstanceProcessorDebugOutput i2 d2 e2 =
        InstanceProcessorDebugOutput (i1 <> i2) (d1 <> d2) (e1 <> e2)

instance Monoid InstanceProcessorDebugOutput where
    mempty = InstanceProcessorDebugOutput mempty mempty mempty

-- | A type of processor of instances
type InstanceProcessor
     = WithDebugOutput InstanceProcessorError InstanceProcessorDebugOutput

-- | Processes instances and generated expressions for them
processInstances ::
       HM.HashMap Ident I.Instance
    -> HM.HashMap Ident C.Class
    -> HM.HashMap Ident C.DefaultInstance
    -> [K.Instance]
    -> ( Either InstanceProcessorError InstanceProcessorOutput
       , InstanceProcessorDebugOutput)
processInstances initialInstances classes defaultInstances instances =
    runWithDebugOutput $
    processInstances' initialInstances classes defaultInstances instances

processInstances' ::
       HM.HashMap Ident I.Instance
    -> HM.HashMap Ident C.Class
    -> HM.HashMap Ident C.DefaultInstance
    -> [K.Instance]
    -> InstanceProcessor InstanceProcessorOutput
processInstances' initialInstances classes defaultInstances instances = do
    instanceMap <- foldM (collectInstance initialInstances) HM.empty instances
    processedDefault <-
        mapHashMapWithKeyM (processDefaultInstance classes) defaultInstances
    let allInstances = initialInstances <> instanceMap
    processedInstances <-
        HM.fromList <$> mapM (processInstance classes allInstances) instances
    return
        InstanceProcessorOutput
            { getInstanceProcessorOutputInstances = instanceMap
            , getInstanceProcessorOutputExpressions =
                  processedDefault <> processedInstances
            }

collectInstance ::
       HM.HashMap Ident I.Instance
    -> HM.HashMap Ident I.Instance
    -> K.Instance
    -> InstanceProcessor (HM.HashMap Ident I.Instance)
collectInstance definedInstances instances inst
    | K.Instance {K.getInstanceClass = className, K.getInstanceType = typeName} <-
         inst =
        let instanceName =
                IdentInstance (getValue className) (getValue typeName)
         in case HM.lookup instanceName (definedInstances <> instances) of
                Just _ ->
                    raiseError $
                    InstanceProcessorErrorAlreadyDefined className typeName
                Nothing -> do
                    let newMap =
                            HM.singleton instanceName $ convertInstance inst
                    writeDebugOutput
                        mempty
                            { getInstanceProcessorDebugOutputInstances =
                                  Just newMap
                            }
                    return $ instances <> newMap

convertInstance :: K.Instance -> I.Instance
convertInstance K.Instance { K.getInstanceContext = context
                           , K.getInstanceClass = className
                           , K.getInstanceType = typeName
                           , K.getInstanceTypeArgs = args
                           } =
    let instanceClass = getValue className
        instanceType = getValue typeName
        instanceName = IdentInstance instanceClass instanceType
        instanceDefaultName = IdentInstance instanceClass instanceClass
     in I.Instance
            { I.getInstanceContext =
                  map removePositionsOfSimpleConstraint context
            , I.getInstanceClass = instanceClass
            , I.getInstanceType = instanceType
            , I.getInstanceTypeArgs = map getValue args
            , I.getInstanceExpression = instanceName
            , I.getInstanceDefaultExpression = instanceDefaultName
            }

processDefaultInstance ::
       HM.HashMap Ident C.Class
    -> Ident
    -> C.DefaultInstance
    -> InstanceProcessor K.Expression
processDefaultInstance classes instanceName C.DefaultInstance { C.getDefaultInstanceClassName = className
                                                              , C.getDefaultInstanceMethods = methods
                                                              } = do
    C.Class { C.getClassContext = context
            , C.getClassDataTypeName = dataTypeName
            , C.getClassMethods = methodNames
            , C.getClassGetters = getters
            } <-
        lookupMapValue
            (InstanceProcessorErrorUnknownClass className)
            className
            classes
    let lookupGetter (SimpleConstraint superClass _) =
            lookupMapValue
                (InstanceProcessorErrorUnknownClassComponent
                     className
                     superClass)
                superClass
                getters
    superClassGetters <- mapM lookupGetter context
    let inputIdent =
            withDummyLocation $ IdentGenerated IdentEnvironmentInstances 0
        inputVar = withDummyLocation $ K.ExpVar inputIdent
        makeSuperClassArg getterName =
            let getter =
                    withDummyLocation . K.ExpVar $ withDummyLocation getterName
             in withDummyLocation $ K.ExpApplication getter (inputVar NE.:| [])
        superClassArgs = map makeSuperClassArg superClassGetters
        lookupMethod methodName =
            lookupMapValue
                (InstanceProcessorErrorUnknownClassComponent
                     className
                     methodName)
                methodName
                methods
    methodArgs <- mapM lookupMethod methodNames
    let applicationExp =
            createInstanceApplication dataTypeName superClassArgs methodArgs
        finalExp =
            withDummyLocation $ K.ExpAbstraction inputIdent applicationExp
        expression =
            K.Expression
                { K.getExpressionName = withDummyLocation instanceName
                , K.getExpressionBody = finalExp
                , K.getExpressionType = Nothing
                }
    writeDebugOutput
        mempty
            { getInstanceProcessorDebugOutputDefaults =
                  Just $ HM.singleton instanceName expression
            }
    return expression

processInstance ::
       HM.HashMap Ident C.Class
    -> HM.HashMap Ident I.Instance
    -> K.Instance
    -> InstanceProcessor (Ident, K.Expression)
processInstance classes instances inst
    | K.Instance { K.getInstanceContext = context
                 , K.getInstanceClass = className
                 , K.getInstanceType = typeName
                 , K.getInstanceTypeArgs = typeParams
                 , K.getInstanceMethods = methods
                 } <- inst = do
        C.Class { C.getClassContext = classContext
                , C.getClassDataTypeName = dataTypeName
                , C.getClassMethods = methodNames
                , C.getClassDefaultInstanceName = defaultInstanceName
                , C.getClassGetters = classGetters
                } <-
            lookupMapValue
                (InstanceProcessorErrorUnknownClass (getValue className))
                (getValue className)
                classes
        let instanceName =
                IdentInstance (getValue className) (getValue typeName)
            makeInstanceArg =
                withDummyLocation . IdentGenerated IdentEnvironmentInstances
            instanceArgs =
                zip
                    (map removePositionsOfSimpleConstraint context)
                    (map makeInstanceArg [0 ..])
        superClassArgs <-
            mapM
                (getInstanceSuperClassArg
                     classes
                     instances
                     (getValue typeName)
                     (map getValue typeParams)
                     instanceArgs)
                classContext
        methodArgs <-
            mapM
                (getInstanceMethodArg
                     (getValue className)
                     classGetters
                     defaultInstanceName
                     instanceName
                     methods)
                methodNames
        let instanceApplication =
                createInstanceApplication dataTypeName superClassArgs methodArgs
            makeLambda (_, ident) res =
                withDummyLocation $ K.ExpAbstraction ident res
            instanceExp = foldr makeLambda instanceApplication instanceArgs
            expression =
                K.Expression
                    { K.getExpressionName = withDummyLocation instanceName
                    , K.getExpressionBody = instanceExp
                    , K.getExpressionType = Nothing
                    }
        writeDebugOutput
            mempty
                { getInstanceProcessorDebugOutputExpressions =
                      Just $ HM.singleton instanceName expression
                }
        return (instanceName, expression)

getInstanceSuperClassArg ::
       HM.HashMap Ident C.Class
    -> HM.HashMap Ident I.Instance
    -> Ident
    -> [Ident]
    -> [(SimpleConstraint, WithLocation Ident)]
    -> SimpleConstraint
    -> InstanceProcessor (WithLocation K.Exp)
getInstanceSuperClassArg classes instances typeName typeParams instanceParams constraint
    | SimpleConstraint className _ <- constraint = do
        let instanceName = IdentInstance className typeName
            instanceExp =
                withDummyLocation . K.ExpVar . withDummyLocation $ instanceName
        I.Instance { I.getInstanceContext = instanceContext
                   , I.getInstanceTypeArgs = instanceTypeArgs
                   } <-
            lookupMapValue
                (InstanceProcessorErrorUnknownInstance className typeName)
                instanceName
                instances
        case instanceContext of
            [] -> return instanceExp
            (f:rest) -> do
                let typeMapping = HM.fromList $ zip instanceTypeArgs typeParams
                    mapConstraint (SimpleConstraint c t) =
                        (c, fromJust $ HM.lookup t typeMapping)
                    mappedArgs = fmap mapConstraint (f NE.:| rest)
                args <-
                    mapM
                        (getInstanceArgs classes instanceName instanceParams)
                        mappedArgs
                return . withDummyLocation $ K.ExpApplication instanceExp args

getInstanceArgs ::
       HM.HashMap Ident C.Class
    -> Ident
    -> [(SimpleConstraint, WithLocation Ident)]
    -> (Ident, Ident)
    -> InstanceProcessor (WithLocation K.Exp)
getInstanceArgs classes instanceName instanceArgs (className, classParam) = do
    let constraints =
            filter
                (\(SimpleConstraint _ param, _) -> param == classParam)
                instanceArgs
        lookupConstraint (SimpleConstraint constraintClassName _, ident) = do
            C.Class {C.getClassGetters = getters} <-
                lookupMapValue
                    (InstanceProcessorErrorUnknownClass constraintClassName)
                    constraintClassName
                    classes
            let constraintExp = withDummyLocation $ K.ExpVar ident
                makeExp getter =
                    let getterExp =
                            withDummyLocation . K.ExpVar $
                            withDummyLocation getter
                     in withDummyLocation $
                        K.ExpApplication getterExp (constraintExp NE.:| [])
            return $
                if constraintClassName == className
                    then Just constraintExp
                    else makeExp <$> HM.lookup className getters
    foundExps <- asum <$> mapM lookupConstraint constraints
    case foundExps of
        Just exp' -> return exp'
        Nothing ->
            raiseError $
            InstanceProcessorErrorUnsatisfiedConstraint instanceName className

getInstanceMethodArg ::
       Ident
    -> HM.HashMap Ident Ident
    -> Ident
    -> Ident
    -> HM.HashMap Ident K.Expression
    -> Ident
    -> InstanceProcessor (WithLocation K.Exp)
getInstanceMethodArg className classGetters defaultInstanceName instanceName methods methodName =
    case HM.lookup methodName methods of
        Just K.Expression {K.getExpressionBody = exp'} -> return exp'
        Nothing -> do
            getter <-
                lookupMapValue
                    (InstanceProcessorErrorUnknownClassComponent
                         className
                         methodName)
                    methodName
                    classGetters
            let defaultInstanceExp =
                    withDummyLocation . K.ExpVar $
                    withDummyLocation defaultInstanceName
                instanceExp =
                    withDummyLocation . K.ExpVar $
                    withDummyLocation instanceName
                defaultInstanceApplication =
                    withDummyLocation $
                    K.ExpApplication defaultInstanceExp (instanceExp NE.:| [])
                getterExp =
                    withDummyLocation . K.ExpVar $ withDummyLocation getter
                getterApplication =
                    withDummyLocation $
                    K.ExpApplication
                        getterExp
                        (defaultInstanceApplication NE.:| [])
            return getterApplication

createInstanceApplication ::
       Ident
    -> [WithLocation K.Exp]
    -> [WithLocation K.Exp]
    -> WithLocation K.Exp
createInstanceApplication dataTypeName superClassArgs methodArgs =
    let allArgs = superClassArgs ++ methodArgs
        constrExp =
            withDummyLocation . K.ExpConstr $ withDummyLocation dataTypeName
     in case allArgs of
            [] -> constrExp
            (f:rest) ->
                withDummyLocation $ K.ExpApplication constrExp (f NE.:| rest)
