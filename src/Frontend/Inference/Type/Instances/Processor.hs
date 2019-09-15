{- |
Module      :  Frontend.Inference.Instances.Processor
Description :  Functions for type checking of instances
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Function for type checking of instances
-}
module Frontend.Inference.Type.Instances.Processor where

import Control.Applicative ((<|>))
import Data.Bifunctor (bimap, first)
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import Data.Tuple (swap)

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Base.Common
import Frontend.Inference.Base.DebugOutput
import Frontend.Inference.Base.Descriptor
import Frontend.Inference.Class
import Frontend.Inference.Constraint
import Frontend.Inference.Equalities (EqualitiesGenerationError(..))
import Frontend.Inference.Expression
import qualified Frontend.Inference.InferenceProcessor as I
import Frontend.Inference.Instance
import Frontend.Inference.Kind.Processor
import Frontend.Inference.Signature
import Frontend.Inference.Util.Debug
import Frontend.Inference.Variables
import Frontend.Syntax.Position (WithLocation(..))

-- | A type of debug output of instance inference
data InstanceInferenceDebugOutput = InstanceInferenceDebugOutput
    { getInstanceInferenceDebugOutputKinds :: Maybe SingleGroupInferenceDebugOutput
    , getInstanceInferenceDebugOutputInstances :: Maybe [InferenceDebugOutput]
    }

instance Semigroup InstanceInferenceDebugOutput where
    InstanceInferenceDebugOutput k1 i1 <> InstanceInferenceDebugOutput k2 i2 =
        InstanceInferenceDebugOutput (k1 <|> k2) (i1 <|> i2)

instance Monoid InstanceInferenceDebugOutput where
    mempty = InstanceInferenceDebugOutput mempty mempty

-- | A processor of instance inference
type Processor = WithDebugOutput InferenceError InstanceInferenceDebugOutput

-- | A processor of inference of a single instance
type SingleInstanceProcessor
     = WithDebugOutput InferenceError InferenceDebugOutput

-- | Infers kinds and types of an instance
inferInstances ::
       (Signatures TypeSignature -> SimpleInfer F.Expressions TypeSignature Exp)
    -> Signatures TypeConstructorSignature
    -> HM.HashMap Ident Class
    -> F.Instances
    -> (Either InferenceError [Instance], InstanceInferenceDebugOutput)
inferInstances infer signatures classes instances =
    runWithDebugOutput $ inferInstances' infer classes signatures instances

-- | Infers kinds and types of an instance
inferInstances' ::
       (Signatures TypeSignature -> SimpleInfer F.Expressions TypeSignature Exp)
    -> HM.HashMap Ident Class
    -> Signatures TypeConstructorSignature
    -> F.Instances
    -> Processor [Instance]
inferInstances' infer classes signatures instances = do
    let wrapper I.SingleGroupInferenceDebugOutput {I.getSingleGroupInferenceDebugOutputSolver = s} =
            mempty
                { getInstanceInferenceDebugOutputKinds =
                      Just mempty {getSingleGroupInferenceDebugOutputSolver = s}
                }
    _ <- wrapDebugOutput wrapper $ checkKindsOfExpressions signatures instances
    let inferSingleInstance' inst =
            runWithDebugOutput $ inferSingleInstance infer classes inst
    wrapDebugOutput
        (\debug ->
             mempty {getInstanceInferenceDebugOutputInstances = Just debug}) .
        first sequence . unzip . map inferSingleInstance' $
        instances

-- | Infers types of methods of a single instance
inferSingleInstance ::
       (Signatures TypeSignature -> SimpleInfer F.Expressions TypeSignature Exp)
    -> HM.HashMap Ident Class
    -> F.Instance
    -> SingleInstanceProcessor Instance
inferSingleInstance infer classes F.Instance { F.getInstanceContext = context
                                             , F.getInstanceClass = className
                                             , F.getInstanceType = typeName
                                             , F.getInstanceTypeArgs = typeArgs
                                             , F.getInstanceMethods = methods
                                             } = do
    let newContext = map convertConstraint context
        newClassName = getValue className
        newTypeName = getValue typeName
        newTypeArgs = map getValue typeArgs
        substitutedMethods =
            getSubstitutedMethodSignatures
                classes
                newClassName
                newTypeName
                newTypeArgs
    (foundMethods, foundSignatures) <-
        wrapEither id $ findSignatures substitutedMethods methods
    let (nameToIdent, identToName) =
            createNameToIdentMappings (HM.keys foundMethods)
        renamedMethods = mapKeys nameToIdent foundMethods
        renamedSignatures = mapKeys nameToIdent foundSignatures
    (signatures, _, _) <- wrapResult $ infer renamedSignatures renamedMethods
    let newMethods = mapKeys identToName signatures
    return $
        Instance
            { getInstanceContext = newContext
            , getInstanceClass = newClassName
            , getInstanceType = newTypeName
            , getInstanceTypeArgs = newTypeArgs
            , getInstanceMethods = newMethods
            }

-- | Substitutes type parameters with a type of an instance
getSubstitutedMethodSignatures ::
       HM.HashMap Ident Class
    -> Ident
    -> Ident
    -> [Ident]
    -> Signatures TypeSignature
getSubstitutedMethodSignatures classes className typeName typeArgs =
    let Class {getClassParam = param, getClassMethods = classMethods} =
            fromJust $ HM.lookup className classes -- Existence was checked earlier
        constrType = TypeConstr typeName
        paramType =
            case map TypeVar typeArgs of
                [] -> constrType
                (f:rest) -> TypeApplication constrType (f NE.:| rest)
        sub = HM.singleton param paramType
     in HM.map (substituteType sub) classMethods

-- | Finds signatures of methods of an instance
findSignatures ::
       Signatures TypeSignature
    -> F.Expressions
    -> Either InferenceError ( HM.HashMap Ident F.Expression
                             , Signatures TypeSignature)
findSignatures signatures expressions =
    let findMethodSignature e@F.Expression {F.getExpressionName = name@(WithLocation name' _)} =
            case HM.lookup name' signatures of
                Just signature ->
                    return
                        ( ( name'
                          , e
                                { F.getExpressionType = Nothing -- It should be Nothing in case of methods
                                })
                        , (name', signature))
                Nothing ->
                    Left .
                    InferenceErrorEqualityGeneration .
                    EqualitiesGenerationErrorUnknownName $
                    name
     in bimap HM.fromList HM.fromList . unzip <$>
        mapM findMethodSignature (HM.elems expressions)

-- | Creates new idents to methods and returns mappings to and from created idents
createNameToIdentMappings :: [Ident] -> (Ident -> Ident, Ident -> Ident)
createNameToIdentMappings names =
    let (nameToIdentMapping, identToNameMapping) =
            bimap HM.fromList HM.fromList . unzip . map (\p -> (p, swap p)) $
            zip names (map (IdentGenerated IdentEnvironmentInstances) [1 ..])
        nameToIdent name = fromJust $ HM.lookup name nameToIdentMapping
        identToName ident = fromJust $ HM.lookup ident identToNameMapping
     in (nameToIdent, identToName)

-- | Maps keys of a hashmap
mapKeys :: (Ident -> Ident) -> HM.HashMap Ident b -> HM.HashMap Ident b
mapKeys f = HM.fromList . map (first f) . HM.toList
