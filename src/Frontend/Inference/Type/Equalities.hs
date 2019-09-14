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
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromJust, fromMaybe)

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Base.Common
import Frontend.Inference.Base.Descriptor
import Frontend.Inference.Base.Variables
import Frontend.Inference.Equalities
import Frontend.Inference.Expression
import Frontend.Inference.Signature
import Frontend.Inference.Variables hiding (Type(..))
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position

-- | Collects equalities between types of expressions
generateEqualitiesForExpressions ::
       Signatures TypeConstructorSignature
    -> EqualitiesBuilder F.Expressions TypeSignature Exp
generateEqualitiesForExpressions tyConSignatures inferTypes env exprs items
    | InferenceEnvironment { getInferenceEnvironmentSignatures = signatures
                           , getInferenceEnvironmentTypeVariables = typeVariables
                           } <- env =
        let localEnvironment =
                emptyEqualitiesGeneratorEnvironment
                    { getExpressionSignatures = signatures
                    , getTypeConstructorSignatures = tyConSignatures
                    }
         in runEqualitiesGenerator
                (generateEqualitiesForExpressions'
                     inferTypes
                     typeVariables
                     exprs
                     items)
                localEnvironment

-- | A function which infers types
type InferTypes = Infer F.Expressions TypeSignature Exp

-- | Collects equalities between types of expressions
generateEqualitiesForExpressions' ::
       InferTypes
    -> TypeVariables
    -> F.Expressions
    -> [Ident]
    -> EqualitiesGenerator (Signatures ((Exp, TypeSignature), [Ident]))
generateEqualitiesForExpressions' inferTypes typeVariables exprs items = do
    let boundVars = HM.toList typeVariables
    defineNewTypeVariables (HM.elems typeVariables)
    definedSignatures <- asks getExpressionSignatures
    let createSignatureForItem name =
            case HM.lookup name definedSignatures of
                Just _ -> return Nothing -- Signature is already defined
                Nothing -> (\sig -> Just (name, sig)) <$> createSignature
    allSignatures <- mapM createSignatureForItem items
    let newSignatures = HM.fromList $ catMaybes allSignatures
    expressions <-
        withExpressions newSignatures . withTypeVariables boundVars $
        HM.fromList <$> mapM (generateEqualitiesForIdent inferTypes exprs) items
    return $ HM.map (\s -> (s, [])) expressions

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

-- | Generates equalities for an ident
generateEqualitiesForIdent ::
       InferTypes
    -> F.Expressions
    -> Ident
    -> EqualitiesGenerator (Ident, (Exp, TypeSignature))
generateEqualitiesForIdent inferTypes exprs name =
    let maybeExpr =
            ((\x -> (name, x)) <$>) . generateEqualitiesForExpression inferTypes <$>
            HM.lookup name exprs
       -- This error should not occur, because we expect that all idents are
       -- defined expressions
     in fromMaybe (error $ "Unexpected identifier " ++ show name) maybeExpr

-- | Generates equalities for an expression
generateEqualitiesForExpression ::
       InferTypes -> F.Expression -> EqualitiesGenerator (Exp, TypeSignature)
generateEqualitiesForExpression inferTypes F.Expression { F.getExpressionName = name
                                                        , F.getExpressionBody = body
                                                        } = do
    (exp', (resultType, resultKind, resultSort)) <-
        generateEqualitiesForExp inferTypes body
    signature <-
        asks $ fromJust . HM.lookup (getValue name) . getExpressionSignatures
    ((expectedType, expectedKind, expectedSort), _) <-
        specialiseExpressionSignature signature
    writeTypeEqualities [(resultType, expectedType)]
    writeKindEqualities [(resultKind, expectedKind)]
    writeSortEqualities [(resultSort, expectedSort)]
    return (exp', signature)

-- | Generates equalities for an expression
generateEqualitiesForExp ::
       InferTypes
    -> WithLocation F.Exp
    -> EqualitiesGenerator (Exp, (Type, Kind, Sort))
generateEqualitiesForExp inferTypes expr =
    case getValue expr of
        F.ExpVar name -> do
            maybeVariable <- lookupTypeVariable name
            case maybeVariable of
                Just res -> return (ExpVar (getValue name), res)
                Nothing -> do
                    (res, external) <- lookupTypeOfExpression name
                    return (ExpExternal external, res)
        F.ExpConstr name -> do
            (res, external) <- lookupTypeOfExpression name
            return (ExpConstr external, res)
        F.ExpConst c ->
            let makeType name =
                    return
                        ( ExpConst (getValue c)
                        , (TypeConstr $ IdentNamed name, KindStar, SortSquare))
             in case getValue c of
                    F.ConstInt _ -> makeType iNT_NAME
                    F.ConstFloat _ -> makeType fLOAT_NAME
                    F.ConstChar _ -> makeType cHAR_NAME
                    F.ConstString _ -> makeType sTRING_NAME
        F.ExpAbstraction var inner -> do
            args <- createNewTypeVariables [getValue var]
            (innerExp, (exprType, exprKind, exprSort)) <-
                withTypeVariables args $
                generateEqualitiesForExp inferTypes inner
            let [(_, (typeVar, kindVar, sortVar))] = args
            writeKindStar [exprKind, kindVar]
            writeSortSquare [exprSort, sortVar]
            return
                ( ExpAbstraction (getValue var) innerExp
                , (TypeFunction typeVar exprType, KindStar, SortSquare))
        F.ExpApplication func args -> do
            (funcExp, (funcType, funcKind, funcSort)) <-
                generateEqualitiesForExp inferTypes func
            argsRes <-
                mapM (generateEqualitiesForExp inferTypes) (NE.toList args)
            (resultType, resultKind, resultSort) <- createNewTypeVariable
            let (argsExp, argsTypes) = unzip argsRes
                (types, kinds, sorts) = unzip3 argsTypes
                expectedType = foldr TypeFunction resultType types
            writeTypeEqualities [(funcType, expectedType)]
            writeKindStar $ funcKind : resultKind : kinds
            writeSortSquare $ funcSort : resultSort : sorts
            return
                ( ExpApplication funcExp (NE.fromList argsExp)
                , (resultType, KindStar, SortSquare))
        F.ExpCase var constr args ifSuccess ifFail -> do
            (variableType, variableKind, variableSort) <-
                lookupTypeOfVariable var
            ((constructorType, constructorKind, constructorSort), constrExternal) <-
                lookupTypeOfExpression constr
            argsVars <- createNewTypeVariables (map getValue args)
            let (types, kinds, sorts) = unzip3 . map snd $ argsVars
                expectedConstructorType = foldr TypeFunction variableType types
            (ifSuccessExp, (ifSuccessType, ifSuccessKind, ifSuccessSort)) <-
                withTypeVariables argsVars $
                generateEqualitiesForExp inferTypes ifSuccess
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
                ( ExpCase
                      (getValue var)
                      constrExternal
                      (map getValue args)
                      ifSuccessExp
                      (getValue ifFail)
                , (resultType, KindStar, SortSquare))
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
            -- TODO: re-implement this method
            _ <- undefined $ \d -> d ++ [debug]
            (inferredDecls, newCounter, equalities) <- undefined result
            liftGen $ put newCounter
            writeTypeVariableEqualitiesMap equalities
            let inferredSignatures = HM.map snd inferredDecls
            (innerExp, res) <-
                local (defineExpressions inferredSignatures) $
                generateEqualitiesForExp inferTypes inner
            return (ExpLet inferredDecls innerExp, res)

-- | Saves equalities between type, kinds and sorts of bound variables
writeTypeVariableEqualitiesMap ::
       TypeVariableEqualitiesMap -> EqualitiesGenerator ()
writeTypeVariableEqualitiesMap = mapM_ writeTypeVariableEqualities . HM.elems

-- | Saves equalities between type, kinds and sorts of a bound variable
writeTypeVariableEqualities :: TypeVariableEqualities -> EqualitiesGenerator ()
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
