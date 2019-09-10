{- |
Module      :  Frontend.Inference.Type.Processor
Description :  Functions for type inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of type inference
-}
module Frontend.Inference.Type.Processor where

import Control.Applicative ((<|>))
import Data.Bifunctor (bimap, second)
import qualified Data.HashMap.Lazy as HM

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Base.Common
import Frontend.Inference.Base.DebugOutput
import Frontend.Inference.Base.Descriptor
import Frontend.Inference.Base.Processor
import Frontend.Inference.Class
import Frontend.Inference.Expression
import Frontend.Inference.Instance
import Frontend.Inference.Signature
import Frontend.Inference.Solver
import Frontend.Inference.Type.Classes
import Frontend.Inference.Type.DataTypes
import Frontend.Inference.Type.Equalities
import Frontend.Inference.Type.Instances.Processor
    ( InstanceInferenceDebugOutput(..)
    , inferInstances
    )
import Frontend.Inference.Type.Signatures
import Frontend.Inference.Type.WithDependencies
import Frontend.Inference.TypeSynonyms.Processor (TypeSynonymSignatures)
import Frontend.Inference.Util.Debug
import Frontend.Inference.Variables

-- | An output of the type inference processor
data TypeSignatures = TypeSignatures
    { getTypeSignaturesConstructors :: HM.HashMap Ident TypeSignature
    , getTypeSignaturesMethods :: HM.HashMap Ident TypeSignature
    , getTypeSignaturesExpressions :: HM.HashMap Ident (Exp, TypeSignature)
    , getTypeSignaturesClasses :: HM.HashMap Ident Class
    , getTypeSignaturesInstances :: [Instance]
    } deriving (Eq, Show)

-- | A debug output of type inference
data TypeInferenceDebugOutput = TypeInferenceDebugOutput
    { getTypeInferenceDebugOutputConstructorsOutput :: Maybe SingleGroupInferenceDebugOutput
    , getTypeInferenceDebugOutputMethodsOutput :: Maybe SingleGroupInferenceDebugOutput
    , getTypeInferenceDebugOutputExpressions :: Maybe InferenceDebugOutput
    , getTypeInferenceDebugOutputClasses :: Maybe (HM.HashMap Ident InferenceDebugOutput)
    , getTypeInferenceDebugOutputInstances :: Maybe InstanceInferenceDebugOutput
    }

instance Semigroup TypeInferenceDebugOutput where
    TypeInferenceDebugOutput c1 m1 e1 cl1 i1 <> TypeInferenceDebugOutput c2 m2 e2 cl2 i2 =
        TypeInferenceDebugOutput
            (c1 <|> c2)
            (m1 <|> m2)
            (e1 <|> e2)
            (cl1 <|> cl2)
            (i1 <|> i2)

instance Monoid TypeInferenceDebugOutput where
    mempty =
        TypeInferenceDebugOutput
            { getTypeInferenceDebugOutputConstructorsOutput = Nothing
            , getTypeInferenceDebugOutputMethodsOutput = Nothing
            , getTypeInferenceDebugOutputExpressions = Nothing
            , getTypeInferenceDebugOutputClasses = Nothing
            , getTypeInferenceDebugOutputInstances = Nothing
            }

-- | An empty type signature
emptyTypeSignatures :: TypeSignatures
emptyTypeSignatures =
    TypeSignatures
        { getTypeSignaturesConstructors = HM.empty
        , getTypeSignaturesMethods = HM.empty
        , getTypeSignaturesExpressions = HM.empty
        , getTypeSignaturesClasses = HM.empty
        , getTypeSignaturesInstances = []
        }

-- | A processor of type inference
type Processor = WithDebugOutput InferenceError TypeInferenceDebugOutput

-- | Infers types of functions in the module
inferTypes ::
       Signatures TypeConstructorSignature
    -> TypeSynonymSignatures
    -> TypeSignatures
    -> F.Module
    -> (Either InferenceError TypeSignatures, TypeInferenceDebugOutput)
inferTypes signatures typeSynonymSignatures typeSignatures module' =
    runWithDebugOutput $
    inferTypes' signatures typeSynonymSignatures typeSignatures module'

-- | Infer types of functions in the module
inferTypes' ::
       Signatures TypeConstructorSignature
    -> TypeSynonymSignatures
    -> TypeSignatures
    -> F.Module
    -> Processor TypeSignatures
inferTypes' signatures typeSynonymSignatures typeSignatures module'
    | TypeSignatures { getTypeSignaturesConstructors = initialConstructorSignatures
                     , getTypeSignaturesMethods = initialMethodSignatures
                     , getTypeSignaturesExpressions = initialExpressionSignatures
                     } <- typeSignatures
    , F.Module { F.getModuleDataTypes = dataTypes
               , F.getModuleClasses = classes
               , F.getModuleExpressions = expressions
               , F.getModuleInstances = instances
               } <- module' = do
        let constructors =
                HM.unions . map createConstructorSignatures . HM.elems $
                dataTypes
            methods = HM.unions . map createClassSignatures . HM.elems $ classes
            inferSignatures =
                inferTypeSignatures' signatures typeSynonymSignatures
        constructorSignatures <-
            mapDebugOutput
                (\debug ->
                     mempty
                         { getTypeInferenceDebugOutputConstructorsOutput =
                               Just debug
                         }) $
            inferSignatures constructors
        methodSignatures <-
            mapDebugOutput
                (\debug ->
                     mempty
                         {getTypeInferenceDebugOutputMethodsOutput = Just debug}) $
            inferSignatures methods
        let descriptor =
                typeInferenceDescriptor signatures typeSynonymSignatures
            preparedTypeSignatures =
                initialConstructorSignatures <> constructorSignatures <>
                initialMethodSignatures <>
                methodSignatures <>
                HM.map snd initialExpressionSignatures
        (expressionSignatures, _, _) <-
            wrapDebugOutput
                (\debug ->
                     mempty
                         {getTypeInferenceDebugOutputExpressions = Just debug}) $
            doTypeInference descriptor preparedTypeSignatures expressions
        let finalTypeSignatures =
                preparedTypeSignatures <> HM.map snd expressionSignatures
        inferredClasses <-
            wrapDebugOutput
                (\debug ->
                     mempty {getTypeInferenceDebugOutputClasses = Just debug}) $
            processMultiple
                (inferClass (doTypeInference descriptor finalTypeSignatures))
                classes
        inferredInstances <-
            wrapDebugOutput
                (\debug ->
                     mempty {getTypeInferenceDebugOutputInstances = Just debug}) $
            inferInstances
                (\extra ->
                     doTypeInference descriptor (finalTypeSignatures <> extra))
                signatures
                inferredClasses
                instances
        return
            TypeSignatures
                { getTypeSignaturesConstructors = constructorSignatures
                , getTypeSignaturesMethods = methodSignatures
                , getTypeSignaturesExpressions = expressionSignatures
                , getTypeSignaturesClasses = inferredClasses
                , getTypeSignaturesInstances = inferredInstances
                }

-- | Describes the process of a type inference
typeInferenceDescriptor ::
       Signatures TypeConstructorSignature
    -> TypeSynonymSignatures
    -> InferenceDescriptor F.Expressions TypeSignature Exp
typeInferenceDescriptor signatures typeSynonyms =
    InferenceDescriptor
        { getInferenceDescriptorSignaturesGetter =
              inferTypeSignatures signatures typeSynonyms
        , getInferenceDescriptorDependenyGraphBuilder =
              getExpressionsDependencyGraph . HM.keysSet
        , getInferenceDescriptorSingleGroup =
              SingleGroupInferenceDescriptor
                  { getSingleGroupInferenceDescriptorEqualitiesBuilder =
                        generateEqualitiesForExpressions signatures
                  , getSingleGroupInferenceDescriptorApplySolution =
                        applySolution
                  }
        }

-- | Does type inference
doTypeInference ::
       InferenceDescriptor a s x -> Signatures s -> SimpleInfer a s x
doTypeInference descriptor sigs =
    runInfer
        descriptor
        (InferenceEnvironment
             { getInferenceEnvironmentSignatures = sigs
             , getInferenceEnvironmentTypeVariables = HM.empty
             })
        emptyVariableGeneratorState

-- | Applies solution of the system of equalities to a pair of
-- | an expression and a type signature
applySolution :: SolutionApplier (Exp, TypeSignature)
applySolution _ eq sol (exp', typeSig) =
    let appliedExp = applyTypeSolution sol exp'
        appliedTypeSig = applyTypeSolutionAndGeneralise eq sol typeSig
        constraints = getRelevantTypeConstraints sol appliedTypeSig
        finalTypeSig = appliedTypeSig {getTypeSignatureContext = constraints}
     in (appliedExp, finalTypeSig)

-- | Does inference of each entry in a provided map and combines results
processMultiple ::
       (a -> BaseInferOutput b)
    -> HM.HashMap Ident a
    -> ( Either InferenceError (HM.HashMap Ident b)
       , HM.HashMap Ident InferenceDebugOutput)
processMultiple f m =
    let processSingle (name, obj) =
            bimap (second (HM.singleton name)) (HM.singleton name) $ f obj
        results = map processSingle $ HM.toList m
        debug = HM.unions $ map snd results
        result = HM.unions <$> mapM fst results
     in (result, debug)
