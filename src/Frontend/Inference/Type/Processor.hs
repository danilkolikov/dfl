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
import Data.Bifunctor (second)
import qualified Data.HashMap.Lazy as HM

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Base.Common
import Frontend.Inference.Base.DebugOutput
import Frontend.Inference.Base.Descriptor
import Frontend.Inference.Base.Processor hiding (writeDebugOutput)
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
    , getTypeSignaturesExpressions :: HM.HashMap Ident TypeSignature
    } deriving (Eq, Show)

-- | A debug output of type inference
data TypeInferenceDebugOutput = TypeInferenceDebugOutput
    { getTypeInferenceDebugOutputConstructorsOutput :: Maybe SingleGroupInferenceDebugOutput
    , getTypeInferenceDebugOutputMethodsOutput :: Maybe SingleGroupInferenceDebugOutput
    , getTypeInferenceDebugOutputExpressions :: Maybe InferenceDebugOutput
    }

instance Semigroup TypeInferenceDebugOutput where
    TypeInferenceDebugOutput c1 m1 e1 <> TypeInferenceDebugOutput c2 m2 e2 =
        TypeInferenceDebugOutput (c1 <|> c2) (m1 <|> m2) (e1 <|> e2)

instance Monoid TypeInferenceDebugOutput where
    mempty =
        TypeInferenceDebugOutput
            { getTypeInferenceDebugOutputConstructorsOutput = Nothing
            , getTypeInferenceDebugOutputMethodsOutput = Nothing
            , getTypeInferenceDebugOutputExpressions = Nothing
            }

-- | An empty type signature
emptyTypeSignatures :: TypeSignatures
emptyTypeSignatures =
    TypeSignatures
        { getTypeSignaturesConstructors = HM.empty
        , getTypeSignaturesMethods = HM.empty
        , getTypeSignaturesExpressions = HM.empty
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
            inferenceEnvironment =
                InferenceEnvironment
                    { getInferenceEnvironmentSignatures =
                          initialConstructorSignatures <> constructorSignatures <>
                          initialMethodSignatures <>
                          methodSignatures <>
                          initialExpressionSignatures
                    , getInferenceEnvironmentTypeVariables = HM.empty
                    }
            (result, debugOutput) =
                runInfer
                    descriptor
                    inferenceEnvironment
                    emptyVariableGeneratorState
                    expressions
        writeDebugOutput
            mempty {getTypeInferenceDebugOutputExpressions = Just debugOutput}
        (expressionSignatures, _, _) <- except result
        return
            TypeSignatures
                { getTypeSignaturesConstructors = constructorSignatures
                , getTypeSignaturesMethods = methodSignatures
                , getTypeSignaturesExpressions = expressionSignatures
                }

-- | Describes the process of a type inference
typeInferenceDescriptor ::
       Signatures TypeConstructorSignature
    -> TypeSynonymSignatures
    -> InferenceDescriptor F.Expressions TypeSignature
typeInferenceDescriptor signatures typeSynonyms =
    InferenceDescriptor
        { getInferenceDescriptorSignaturesGetter =
              inferTypeSignatures signatures typeSynonyms
        , getInferenceDescriptorDependenyGraphBuilder =
              getExpressionsDependencyGraph . HM.keysSet
        , getInferenceDescriptorSingleGroup =
              SingleGroupInferenceDescriptor
                  { getSingleGroupInferenceDescriptorEqualitiesBuilder =
                        generateEqualitiesForExpressions
                  , getSingleGroupInferenceDescriptorApplySolution =
                        const applyTypeSolution -- Ignore type variables
                  }
        }
