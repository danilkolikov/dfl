{- |
Module      :  Frontend.Inference.Type.Equalities
Description :  Function for generation of type equalities
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for generation of equalities between types, kinds and sorts
-}
module Frontend.Inference.Type.Equalities where

import Control.Monad.Trans.Reader (asks)
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)

import Frontend.Inference.Equalities
import qualified Frontend.Inference.Expression as T
import qualified Frontend.Inference.Let.Ast as L
import Frontend.Inference.Signature
import Frontend.Inference.Substitution
import Frontend.Inference.Util.HashMap
import Frontend.Inference.Variables hiding (Type(..))
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position

-- | Collects equalities between types of expressions
generateEqualitiesForExpressions ::
       L.Expressions
    -> EqualitiesGenerator (Signatures ( (T.Exp, TypeSignature)
                                       , (Substitution Type, Substitution Kind)
                                       , Maybe TypeSignature))
generateEqualitiesForExpressions exprs = do
    let createSignatureForExp L.Expression {L.getExpressionType = signature} =
            case signature of
                Just sig -> return sig
                Nothing -> createSignature
    newSignatures <- mapHashMapM createSignatureForExp exprs
    withExpressions newSignatures $
        mapHashMapM generateEqualitiesForExpression exprs

-- | Creates a type signatures
createSignature :: EqualitiesGenerator TypeSignature
createSignature = do
    (resultType, resultKind, resultSort) <- createNewTypeVariable
    return $
        TypeSignature
            { getTypeSignatureSort = resultSort
            , getTypeSignatureKindParams = []
            , getTypeSignatureKind = resultKind
            , getTypeSignatureTypeParams = []
            , getTypeSignatureType = resultType
            , getTypeSignatureContext = []
            }

-- | Generates equalities for an expression
generateEqualitiesForExpression ::
       L.Expression
    -> EqualitiesGenerator ( (T.Exp, TypeSignature)
                           , (Substitution Type, Substitution Kind)
                           , Maybe TypeSignature)
generateEqualitiesForExpression L.Expression { L.getExpressionName = name
                                             , L.getExpressionBody = body
                                             , L.getExpressionType = type'
                                             } = do
    (exp', (resultType, resultKind, resultSort)) <-
        generateEqualitiesForExp body
    signature <-
        asks $ fromJust . HM.lookup (getValue name) . getExpressionSignatures
    ((expectedType, expectedKind, expectedSort, expectedConstraints), subs) <-
        specialiseExpressionSignature signature
    writeTypeEqualities [(resultType, expectedType)]
    writeKindEqualities [(resultKind, expectedKind)]
    writeSortEqualities [(resultSort, expectedSort)]
    let expectedSignature =
            TypeSignature
                { getTypeSignatureSort = expectedSort
                , getTypeSignatureKindParams = []
                , getTypeSignatureKind = expectedKind
                , getTypeSignatureTypeParams = []
                , getTypeSignatureType = expectedType
                , getTypeSignatureContext = expectedConstraints
                }
    return ((exp', expectedSignature), subs, type')

-- | Generates equalities for an expression
generateEqualitiesForExp ::
       WithLocation L.Exp -> EqualitiesGenerator (T.Exp, (Type, Kind, Sort))
generateEqualitiesForExp expr =
    case getValue expr of
        L.ExpVar name -> do
            maybeVariable <- lookupTypeVariable name
            case maybeVariable of
                Just res -> return (T.ExpVar (getValue name), res)
                Nothing -> do
                    (res, external) <- lookupTypeOfExpression name
                    return (T.ExpExternal external, res)
        L.ExpConstr name -> do
            (res, external) <- lookupTypeOfExpression name
            return (T.ExpConstr external, res)
        L.ExpConst c ->
            let makeType name =
                    return
                        ( T.ExpConst (getValue c)
                        , (TypeConstr $ IdentNamed name, KindStar, SortSquare))
             in case getValue c of
                    L.ConstInt _ -> makeType iNT_NAME
                    L.ConstFloat _ -> makeType fLOAT_NAME
                    L.ConstChar _ -> makeType cHAR_NAME
                    L.ConstString _ -> makeType sTRING_NAME
        L.ExpAbstraction var inner -> do
            args <- createNewTypeVariables [getValue var]
            (innerExp, (exprType, exprKind, exprSort)) <-
                withTypeVariables args $ generateEqualitiesForExp inner
            let [(_, (typeVar, kindVar, sortVar))] = args
            writeKindStar [exprKind, kindVar]
            writeSortSquare [exprSort, sortVar]
            return
                ( T.ExpAbstraction (getValue var) innerExp
                , (TypeFunction typeVar exprType, KindStar, SortSquare))
        L.ExpApplication func args -> do
            (funcExp, (funcType, funcKind, funcSort)) <-
                generateEqualitiesForExp func
            argsRes <- mapM generateEqualitiesForExp (NE.toList args)
            (resultType, resultKind, resultSort) <- createNewTypeVariable
            let (argsExp, argsTypes) = unzip argsRes
                (types, kinds, sorts) = unzip3 argsTypes
                expectedType = foldr TypeFunction resultType types
            writeTypeEqualities [(funcType, expectedType)]
            writeKindStar $ funcKind : resultKind : kinds
            writeSortSquare $ funcSort : resultSort : sorts
            return
                ( T.ExpApplication funcExp (NE.fromList argsExp)
                , (resultType, KindStar, SortSquare))
        L.ExpCase var constr args ifSuccess ifFail -> do
            (variableType, variableKind, variableSort) <-
                lookupTypeOfVariable var
            ((constructorType, constructorKind, constructorSort), constrExternal) <-
                lookupTypeOfExpression constr
            argsVars <- createNewTypeVariables (map getValue args)
            let (types, kinds, sorts) = unzip3 . map snd $ argsVars
                expectedConstructorType = foldr TypeFunction variableType types
            (ifSuccessExp, (ifSuccessType, ifSuccessKind, ifSuccessSort)) <-
                withTypeVariables argsVars $ generateEqualitiesForExp ifSuccess
            (ifFailType, ifFailKind, ifFailSort) <- lookupTypeOfVariable ifFail
            (resultType, resultKind, resultSort) <- createNewTypeVariable
            writeTypeEqualities
                [ (constructorType, expectedConstructorType)
                , (resultType, ifSuccessType)
                , (resultType, ifFailType)
                ]
            writeKindStar $
                [ variableKind
                , constructorKind
                , ifSuccessKind
                , ifFailKind
                , resultKind
                ] ++
                kinds
            writeSortSquare $
                [ variableSort
                , constructorSort
                , ifSuccessSort
                , ifFailSort
                , resultSort
                ] ++
                sorts
            return
                ( T.ExpCase
                      (getValue var)
                      constrExternal
                      (map getValue args)
                      ifSuccessExp
                      (getValue ifFail)
                , (resultType, KindStar, SortSquare))
