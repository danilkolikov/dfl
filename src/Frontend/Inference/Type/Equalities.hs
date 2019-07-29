{- |
Module      :  Frontend.Inference.Type.Equalities
Description :  Function for generation of type equalities
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for generation of equalities between types, kinds and sorts
-}
module Frontend.Inference.Type.Equalities where

import Control.Monad.Trans.Reader (ask, asks, local)
import Control.Monad.Trans.State.Lazy (get, put)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromJust, fromMaybe, mapMaybe)

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Base.Common
import Frontend.Inference.Base.Descriptor
import Frontend.Inference.Base.Variables
import Frontend.Inference.Equalities
import Frontend.Inference.Signature
import Frontend.Inference.Variables hiding (Type(..))
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position

-- | Collects equalities between types of expressions
generateEqualitiesForExpressions ::
       EqualitiesBuilder F.Expressions (Signatures TypeSignature)
generateEqualitiesForExpressions inferTypes env exprs items
    | InferenceEnvironment { getInferenceEnvironmentSignatures = signatures
                           , getInferenceEnvironmentTypeVariables = typeVariables
                           } <- env =
        let localEnvironment =
                emptyEqualitiesGeneratorEnvironment
                    {getExpressionSignatures = signatures}
         in runEqualitiesGenerator
                (generateEqualitiesForExpressions'
                     inferTypes
                     typeVariables
                     exprs
                     items)
                localEnvironment
                []

-- | A function which infers types
type InferTypes = Infer F.Expressions (Signatures TypeSignature)

-- | Collects equalities between types of expressions
generateEqualitiesForExpressions' ::
       InferTypes
    -> TypeVariables
    -> F.Expressions
    -> [Ident]
    -> InferenceEqualitiesGenerator (Signatures TypeSignature)
generateEqualitiesForExpressions' inferTypes typeVariables exprs items = do
    let boundVars = HM.toList typeVariables
    defineNewVariables boundVars
    definedSignatures <- asks getExpressionSignatures
    let createSignatureForItem name =
            case HM.lookup name definedSignatures of
                Just _ -> Nothing -- Signature was inferred earlier
                Nothing -> Just $ (\sig -> (name, sig)) <$> createSignature
        signatureCreators = mapMaybe createSignatureForItem items
    signatures <- HM.fromList <$> sequence signatureCreators
    withExpressions signatures . withTypeVariables boundVars $
        mapM_ (generateEqualitiesForIdent inferTypes exprs) items
    return signatures

-- | Creates a type signatures
createSignature :: InferenceEqualitiesGenerator TypeSignature
createSignature =
    liftGen $ do
        resultType <- generateTypeVariable
        resultKind <- generateKindVariable
        resultSort <- generateSortVariable
        return $
            TypeSignature
                { getTypeSignatureSort = resultSort
                , getTypeSignatureKindParams = []
                , getTypeSignatureKind = resultKind
                , getTypeSignatureTypeParams = []
                , getTypeSignatureType = resultType
                , getTypeSignatureContext = []
                }

-- | Generates equalities for an ident
generateEqualitiesForIdent ::
       InferTypes -> F.Expressions -> Ident -> InferenceEqualitiesGenerator ()
generateEqualitiesForIdent inferTypes exprs name =
    let maybeExpr =
            generateEqualitiesForExpression inferTypes <$> HM.lookup name exprs
       -- This error should not occur, because we expect that all idents are
       -- defined expressions
     in fromMaybe (error $ "Unexpected identifier " ++ show name) maybeExpr

-- | Generates equalities for an expression
generateEqualitiesForExpression ::
       InferTypes -> F.Expression -> InferenceEqualitiesGenerator ()
generateEqualitiesForExpression inferTypes F.Expression { F.getExpressionName = name
                                                        , F.getExpressionBody = body
                                                        } = do
    resultType <- generateEqualitiesForExp inferTypes body
    signature <-
        asks $ fromJust . HM.lookup (getValue name) . getExpressionSignatures
    expectedType <- specialiseExpressionSignature signature
    writeTypeEqualities [(resultType, expectedType)]

-- | Generates equalities for an expression
generateEqualitiesForExp ::
       InferTypes -> WithLocation F.Exp -> InferenceEqualitiesGenerator Type
generateEqualitiesForExp inferTypes expr =
    case getValue expr of
        F.ExpVar name -> lookupTypeOfVariableOrExpression name
        F.ExpConstr name -> lookupTypeOfVariableOrExpression name
        F.ExpConst c ->
            let makeType = return . TypeConstr . IdentNamed
             in case getValue c of
                    F.ConstInt _ -> makeType iNT_NAME
                    F.ConstFloat _ -> makeType fLOAT_NAME
                    F.ConstChar _ -> makeType cHAR_NAME
                    F.ConstString _ -> makeType sTRING_NAME
        F.ExpAbstraction var inner -> do
            args <- createNewVariables [getValue var]
            exprType <-
                withTypeVariables args $
                generateEqualitiesForExp inferTypes inner
            let [(_, (typeVar, _, _))] = args
            return $ TypeFunction typeVar exprType
        F.ExpApplication func args -> do
            funcType <- generateEqualitiesForExp inferTypes func
            argsTypes <- mapM (generateEqualitiesForExp inferTypes) args
            resultType <- liftGen generateTypeVariable
            let expectedType = foldr TypeFunction resultType argsTypes
            writeTypeEqualities [(funcType, expectedType)]
            return resultType
        F.ExpCase var constr args ifSuccess ifFail -> do
            variableType <- lookupTypeOfVariableOrExpression var
            constructorType <- lookupTypeOfVariableOrExpression constr
            argsVars <- createNewVariables (map getValue args)
            let argsTypes = map (\(name, (t, _, _)) -> (name, t)) argsVars
                expectedConstructorType =
                    foldr (TypeFunction . snd) variableType argsTypes
            ifSuccessType <-
                withTypeVariables argsVars $
                generateEqualitiesForExp inferTypes ifSuccess
            ifFailType <- lookupTypeOfVariableOrExpression ifFail
            resultType <- liftGen generateTypeVariable
            writeTypeEqualities
                [ (constructorType, expectedConstructorType)
                , (resultType, ifSuccessType)
                , (resultType, ifFailType)
                ]
            return resultType
        F.ExpLet decls inner -> do
            counter <- liftGen get
            EqualitiesGeneratorEnvironment { getExpressionSignatures = signatures
                                           , getTypeVariables = typeVariables
                                           } <- ask
            let inferenceEnvironment =
                    InferenceEnvironment
                        { getInferenceEnvironmentSignatures = signatures
                        , getInferenceEnvironmentTypeVariables = typeVariables
                        }
                (result, debug) = inferTypes inferenceEnvironment counter decls
            modifyDebugOutput $ \d -> d ++ [debug]
            (inferredDecls, newCounter, equalities) <- wrapNestedError result
            liftGen $ put newCounter
            writeTypeVariableEqualitiesMap equalities
            local (defineExpressions inferredDecls) $
                generateEqualitiesForExp inferTypes inner

-- | Saves equalities between type, kinds and sorts of bound variables
writeTypeVariableEqualitiesMap ::
       TypeVariableEqualitiesMap -> InferenceEqualitiesGenerator ()
writeTypeVariableEqualitiesMap = mapM_ writeTypeVariableEqualities . HM.elems

-- | Saves equalities between type, kinds and sorts of a bound variable
writeTypeVariableEqualities ::
       TypeVariableEqualities -> InferenceEqualitiesGenerator ()
writeTypeVariableEqualities TypeVariableEqualities { getTypeVariableEqualitiesTypes = typeEqs
                                                   , getTypeVariableEqualitiesKinds = kindEqs
                                                   , getTypeVariableEqualitiesSorts = sortEqs
                                                   } =
    let createEqualities xs =
            case xs of
                f:rest -> map (\x -> (f, x)) rest
                _ -> []
     in do writeTypeEqualities (createEqualities typeEqs)
           writeKindEqualities (createEqualities kindEqs)
           writeSortEqualities (createEqualities sortEqs)
