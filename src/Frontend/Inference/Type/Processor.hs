{- |
Module      :  Frontend.Inference.Type.Processor
Description :  Functions for type inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of type inference
-}
module Frontend.Inference.Type.Processor where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Control.Monad.Trans.Writer.Lazy (Writer, runWriter, tell)
import Data.Bifunctor (bimap, second)
import qualified Data.HashMap.Lazy as HM

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Base.Common
import Frontend.Inference.Base.DebugOutput
import Frontend.Inference.Base.Descriptor
import Frontend.Inference.Base.Processor hiding (writeDebugOutput)
import Frontend.Inference.Class
import Frontend.Inference.Expression
import Frontend.Inference.Signature
import Frontend.Inference.Solver hiding (writeDebugOutput)
import Frontend.Inference.Type.Classes
import Frontend.Inference.Type.DataTypes
import Frontend.Inference.Type.Equalities
import Frontend.Inference.Type.Signatures
import Frontend.Inference.Type.WithDependencies
import Frontend.Inference.TypeSynonyms.Processor (TypeSynonymSignatures)
import Frontend.Inference.Variables

-- | An output of the type inference processor
data TypeSignatures = TypeSignatures
    { getTypeSignaturesConstructors :: HM.HashMap Ident TypeSignature
    , getTypeSignaturesMethods :: HM.HashMap Ident TypeSignature
    , getTypeSignaturesExpressions :: HM.HashMap Ident (Exp, TypeSignature)
    , getTypeSignaturesClasses :: HM.HashMap Ident Class
    } deriving (Eq, Show)

-- | A debug output of type inference
data TypeInferenceDebugOutput = TypeInferenceDebugOutput
    { getTypeInferenceDebugOutputConstructorsOutput :: Maybe SingleGroupInferenceDebugOutput
    , getTypeInferenceDebugOutputMethodsOutput :: Maybe SingleGroupInferenceDebugOutput
    , getTypeInferenceDebugOutputExpressions :: Maybe InferenceDebugOutput
    , getTypeInferenceDebugOutputClasses :: Maybe (HM.HashMap Ident InferenceDebugOutput)
    }

instance Semigroup TypeInferenceDebugOutput where
    TypeInferenceDebugOutput c1 m1 e1 cl1 <> TypeInferenceDebugOutput c2 m2 e2 cl2 =
        TypeInferenceDebugOutput
            (c1 <|> c2)
            (m1 <|> m2)
            (e1 <|> e2)
            (cl1 <|> cl2)

instance Monoid TypeInferenceDebugOutput where
    mempty =
        TypeInferenceDebugOutput
            { getTypeInferenceDebugOutputConstructorsOutput = Nothing
            , getTypeInferenceDebugOutputMethodsOutput = Nothing
            , getTypeInferenceDebugOutputExpressions = Nothing
            , getTypeInferenceDebugOutputClasses = Nothing
            }

-- | An empty type signature
emptyTypeSignatures :: TypeSignatures
emptyTypeSignatures =
    TypeSignatures
        { getTypeSignaturesConstructors = HM.empty
        , getTypeSignaturesMethods = HM.empty
        , getTypeSignaturesExpressions = HM.empty
        , getTypeSignaturesClasses = HM.empty
        }

-- | A processor of type inference
type Processor = ExceptT InferenceError (Writer [TypeInferenceDebugOutput])

-- | Writes debug output
writeDebugOutput :: TypeInferenceDebugOutput -> Processor ()
writeDebugOutput = lift . tell . return

-- | Infers types of functions in the module
inferTypes ::
       Signatures TypeConstructorSignature
    -> TypeSynonymSignatures
    -> TypeSignatures
    -> F.Module
    -> (Either InferenceError TypeSignatures, TypeInferenceDebugOutput)
inferTypes signatures typeSynonymSignatures typeSignatures module' =
    let processor =
            inferTypes' signatures typeSynonymSignatures typeSignatures module'
        result = runWriter $ runExceptT processor
     in second mconcat result

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
               } <- module' = do
        let constructors =
                HM.unions . map createConstructorSignatures . HM.elems $
                dataTypes
            methods = HM.unions . map createClassSignatures . HM.elems $ classes
            inferSignatures =
                inferTypeSignatures' signatures typeSynonymSignatures
            (inferConstructors, constructorOutput) =
                inferSignatures constructors
            (inferMethods, methodsOutput) = inferSignatures methods
        writeDebugOutput
            mempty
                { getTypeInferenceDebugOutputConstructorsOutput =
                      Just constructorOutput
                , getTypeInferenceDebugOutputMethodsOutput = Just methodsOutput
                }
        constructorSignatures <- except inferConstructors
        methodSignatures <- except inferMethods
        let descriptor =
                typeInferenceDescriptor signatures typeSynonymSignatures
            preparedTypeSignatures =
                initialConstructorSignatures <> constructorSignatures <>
                initialMethodSignatures <>
                methodSignatures <>
                HM.map snd initialExpressionSignatures
            (result, debugOutput) =
                doTypeInference descriptor preparedTypeSignatures expressions
        writeDebugOutput
            mempty {getTypeInferenceDebugOutputExpressions = Just debugOutput}
        (expressionSignatures, _, _) <- except result
        let finalTypeSignatures =
                preparedTypeSignatures <> HM.map snd expressionSignatures
            (classResult, classDebugOutput) =
                processMultiple
                    (inferClass (doTypeInference descriptor finalTypeSignatures))
                    classes
        writeDebugOutput
            mempty {getTypeInferenceDebugOutputClasses = Just classDebugOutput}
        inferredClasses <- except classResult
        return
            TypeSignatures
                { getTypeSignaturesConstructors = constructorSignatures
                , getTypeSignaturesMethods = methodSignatures
                , getTypeSignaturesExpressions = expressionSignatures
                , getTypeSignaturesClasses = inferredClasses
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
