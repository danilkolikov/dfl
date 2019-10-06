{- |
Module      :  Frontend.Inference.Processor
Description :  Processors of kind and type inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processors of kind and type inference
-}
module Frontend.Inference.Processor
    ( processModule
    , InferenceProcessorOutput(..)
    , defaultInferenceProcessorOutput
    , InferenceProcessorError(..)
    , InferenceError(..)
    , SignatureCheckError(..)
    , KindProcessorError(..)
    , TypeSynonymProcessorError(..)
    , ClassProcessorError(..)
    , DataTypeProcessorError(..)
    , InstanceProcessorError(..)
    , TranslationProcessorError(..)
    , InferenceProcessorDebugOutput(..)
    , KindProcessorDebugOutput(..)
    , TypeSynonymProcessorDebugOutput(..)
    , ClassProcessorDebugOutput(..)
    , DataTypeProcessorDebugOutput(..)
    , InstanceProcessorDebugOutput(..)
    , TypeInferenceDebugOutput
    , TranslationProcessorDebugOutput(..)
    ) where

import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Final.Ast (Ident)
import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.BuiltIns
import qualified Frontend.Inference.Class as C
import Frontend.Inference.Class.Processor
import Frontend.Inference.DataType.Processor
import qualified Frontend.Inference.Expression as T
import Frontend.Inference.InferenceProcessor
    ( InferenceError(..)
    , SignatureCheckError(..)
    )
import qualified Frontend.Inference.Instance as I
import Frontend.Inference.Instance.Processor
import qualified Frontend.Inference.Kind.Ast as K
import Frontend.Inference.Kind.Processor
import qualified Frontend.Inference.Let.Ast as L
import Frontend.Inference.Let.Processor
import Frontend.Inference.Signature
import Frontend.Inference.Translation.Processor
import Frontend.Inference.Type.Processor
import Frontend.Inference.TypeSynonym.Processor
import Util.Debug

-- | Errors which can be encounterd during inference
data InferenceProcessorError
    = InferenceProcessorErrorKind KindProcessorError -- ^ Errors of kind inference
    | InferenceProcessorErrorTypeSynonym TypeSynonymProcessorError -- ^ Errors of type synonym expanding
    | InferenceProcessorErrorClass ClassProcessorError -- ^ Errors of class processing
    | InferenceProcessorErrorDataType DataTypeProcessorError -- ^ Errors of data type processing
    | InferenceProcessorErrorInstance InstanceProcessorError -- ^ Errors of instance processing
    | InferenceProcessorErrorType InferenceError -- ^ Errors of type inference
    | InferenceProcessorErrorTranslation TranslationProcessorError -- ^ Errors of expression translation
    deriving (Eq, Show)

-- | Debug output of inference processing
data InferenceProcessorDebugOutput = InferenceProcessorDebugOutput
    { getInferenceProcessorDebugOutputKinds :: Maybe KindProcessorDebugOutput
    , getInferenceProcessorDebugOutputTypeSynonyms :: Maybe TypeSynonymProcessorDebugOutput
    , getInferenceProcessorDebugOutputClasses :: Maybe ClassProcessorDebugOutput
    , getInferenceProcessorDebugOutputDataTypes :: Maybe DataTypeProcessorDebugOutput
    , getInferenceProcessorDebugOutputInstances :: Maybe InstanceProcessorDebugOutput
    , getInferenceProcessorDebugOutputLet :: Maybe (HM.HashMap Ident L.Expression)
    , getInferenceProcessorDebugOutputType :: Maybe TypeInferenceDebugOutput
    , getInferenceProcessorDebugOutputTranslation :: Maybe TranslationProcessorDebugOutput
    } deriving (Eq, Show)

instance Semigroup InferenceProcessorDebugOutput where
    InferenceProcessorDebugOutput k1 ts1 c1 d1 i1 l1 t1 tr1 <> InferenceProcessorDebugOutput k2 ts2 c2 d2 i2 l2 t2 tr2 =
        InferenceProcessorDebugOutput
            (k1 <> k2)
            (ts1 <> ts2)
            (c1 <> c2)
            (d1 <> d2)
            (i1 <> i2)
            (l1 <> l2)
            (t1 <> t2)
            (tr1 <> tr2)

instance Monoid InferenceProcessorDebugOutput where
    mempty =
        InferenceProcessorDebugOutput
            mempty
            mempty
            mempty
            mempty
            mempty
            mempty
            mempty
            mempty

-- | Output of inference processing
data InferenceProcessorOutput = InferenceProcessorOutput
    { getInferenceProcessorOutputTypeConstructors :: Signatures TypeConstructorSignature
    , getInferenceProcessorOutputTypeSynonyms :: Signatures TypeSignature
    , getInferenceProcessorOutputClasses :: HM.HashMap Ident C.Class
    , getInferenceProcessorOutputInstances :: HM.HashMap Ident I.Instance
    , getInferenceProcessorOutputConstructors :: HM.HashMap Ident TypeSignature
    , getInferenceProcessorOutputMethods :: HM.HashMap Ident TypeSignature
    , getInferenceProcessorOutputExpressions :: HM.HashMap Ident T.ExpWithSignature
    } deriving (Eq, Show)

-- | A default output of the processor
defaultInferenceProcessorOutput :: InferenceProcessorOutput
defaultInferenceProcessorOutput =
    InferenceProcessorOutput
        { getInferenceProcessorOutputTypeConstructors = defaultKindSignatures
        , getInferenceProcessorOutputTypeSynonyms = defaultTypeSynonyms
        , getInferenceProcessorOutputClasses = HM.empty
        , getInferenceProcessorOutputInstances = HM.empty
        , getInferenceProcessorOutputConstructors = defaultConstructors
        , getInferenceProcessorOutputMethods = HM.empty
        , getInferenceProcessorOutputExpressions =
              HM.map (\sig -> (undefined, sig)) defaultExpressions
        }

type InferenceProcessor
     = WithDebugOutput InferenceProcessorError InferenceProcessorDebugOutput

-- | Processes a module and infers type, kind and sort information
processModule ::
       InferenceProcessorOutput
    -> F.Module F.Exp
    -> ( Either InferenceProcessorError InferenceProcessorOutput
       , InferenceProcessorDebugOutput)
processModule initialState module' =
    runWithDebugOutput $ processModule' initialState module'

processModule' ::
       InferenceProcessorOutput
    -> F.Module F.Exp
    -> InferenceProcessor InferenceProcessorOutput
processModule' initialState module'
    | InferenceProcessorOutput { getInferenceProcessorOutputTypeConstructors = initialTypeConstructors
                               , getInferenceProcessorOutputTypeSynonyms = initialTypeSynonyms
                               , getInferenceProcessorOutputClasses = initialClasses
                               , getInferenceProcessorOutputInstances = initialInstances
                               , getInferenceProcessorOutputConstructors = initialConstructors
                               , getInferenceProcessorOutputMethods = initialMethods
                               , getInferenceProcessorOutputExpressions = initialExpWithSignatures
                               } <- initialState
        -- Infer kinds for data types
     = do
        (newTypeConstructors, astWithKinds) <-
            wrapErrorAndDebugOutput
                InferenceProcessorErrorKind
                (\debug ->
                     mempty {getInferenceProcessorDebugOutputKinds = Just debug}) $
            inferKinds initialTypeConstructors module'
        let allTypeConstructors = initialTypeConstructors <> newTypeConstructors
        -- Replace type synonyms in the module
        (newTypeSynonyms, K.AstWithKinds { K.getAstWithKindsClasses = astClasses
                                         , K.getAstWithKindsDataTypes = astDataTypes
                                         , K.getAstWithKindsInstances = astInstances
                                         , K.getAstWithKindsExpressions = astExpressions
                                         }) <-
            wrapErrorAndDebugOutput
                InferenceProcessorErrorTypeSynonym
                (\debug ->
                     mempty
                         { getInferenceProcessorDebugOutputTypeSynonyms =
                               Just debug
                         }) $
            processTypeSynonyms
                allTypeConstructors
                initialTypeSynonyms
                (F.getModuleTypeSynonyms module')
                astWithKinds
        -- Generate types for classes
        ClassProcessorOutput { getClassProcessorOutputClasses = newClasses
                             , getClassProcessorOutputDataTypes = classDataTypes
                             , getClassProcessorOutputSignatures = classDataTypeSignatures
                             , getClassProcessorOutputDefaultInstances = defaultInstances
                             , getClassProcessorOutputMethods = classMethods
                             , getClassProcessorOutputGetters = classGetters
                             } <-
            wrapErrorAndDebugOutput
                InferenceProcessorErrorClass
                (\debug ->
                     mempty
                         {getInferenceProcessorDebugOutputClasses = Just debug}) $
            processClasses initialClasses allTypeConstructors astClasses
        -- Generate instances for data types
        let allClasses = initialClasses <> newClasses
            typeConstructorsWithClasses =
                allTypeConstructors <> classDataTypeSignatures
            dataTypesWithClasses = astDataTypes <> classDataTypes
        DataTypeProcessorOutput { getDataTypeProcessorOutputConstructors = constructors
                                , getDataTypeProcessorOutputInstances = dataTypeInstances
                                } <-
            wrapErrorAndDebugOutput
                InferenceProcessorErrorDataType
                (\debug ->
                     mempty
                         { getInferenceProcessorDebugOutputDataTypes =
                               Just debug
                         }) $
            processDataTypes typeConstructorsWithClasses dataTypesWithClasses
        -- Generate expressions for instances
        let allConstructors = initialConstructors <> constructors
            instancesWithDataTypes = astInstances <> dataTypeInstances
        InstanceProcessorOutput { getInstanceProcessorOutputInstances = instanceMap
                                , getInstanceProcessorOutputExpressions = instanceExpressions
                                } <-
            wrapErrorAndDebugOutput
                InferenceProcessorErrorInstance
                (\debug ->
                     mempty
                         { getInferenceProcessorDebugOutputInstances =
                               Just debug
                         }) $
            processInstances
                typeConstructorsWithClasses
                initialInstances
                allClasses
                defaultInstances
                instancesWithDataTypes
        -- Get rid of let expressions
        let allExpressions =
                astExpressions <> classGetters <> instanceExpressions
            expressionsWithoutLet = processExpressions allExpressions
        writeDebugOutput
            mempty
                { getInferenceProcessorDebugOutputLet =
                      Just expressionsWithoutLet
                }
        let allMethods = initialMethods <> classMethods
            allSignatures =
                HM.map snd initialExpWithSignatures <> allMethods <>
                allConstructors
        -- Infer types for expressions
        newExpWithTypes <-
            wrapErrorAndDebugOutput
                InferenceProcessorErrorType
                (\debug ->
                     mempty {getInferenceProcessorDebugOutputType = Just debug}) $
            inferTypesOfExpressions allSignatures expressionsWithoutLet
        -- Translate polymorphic functions
        let allInstances = initialInstances <> instanceMap
        translatedExpWithTypes <-
            wrapErrorAndDebugOutput
                InferenceProcessorErrorTranslation
                (\debug ->
                     mempty
                         { getInferenceProcessorDebugOutputTranslation =
                               Just debug
                         }) $
            translateExpressions
                allClasses
                allInstances
                allSignatures
                newExpWithTypes
        return
            InferenceProcessorOutput
                { getInferenceProcessorOutputTypeConstructors =
                      newTypeConstructors
                , getInferenceProcessorOutputTypeSynonyms = newTypeSynonyms
                , getInferenceProcessorOutputClasses = newClasses
                , getInferenceProcessorOutputInstances = instanceMap
                , getInferenceProcessorOutputConstructors = constructors
                , getInferenceProcessorOutputMethods = classMethods
                , getInferenceProcessorOutputExpressions =
                      translatedExpWithTypes
                }
