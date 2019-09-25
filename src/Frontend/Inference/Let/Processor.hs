{- |
Module      :  Frontend.Inference.Let.Processor
Description :  Functions for "desugaring" of let expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for extracting let expressions into separate expressions
-}
module Frontend.Inference.Let.Processor
    ( processExpressions
    ) where

import Control.Monad (liftM2)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.Trans.State.Lazy (State, evalState, get, modify)
import Control.Monad.Trans.Writer (WriterT, execWriterT, tell)
import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)

import Frontend.Desugaring.Final.Ast (Ident(..), IdentEnvironment(..))
import qualified Frontend.Inference.Kind.Ast as K
import qualified Frontend.Inference.Let.Ast as L
import Frontend.Inference.Signature
import Frontend.Inference.Util.HashMap
import Frontend.Inference.WithVariables
import Frontend.Syntax.Position

-- | Processes expressions and extracts expressions, nested in let blocks, into the global scope
processExpressions ::
       HM.HashMap Ident K.Expression -> HM.HashMap Ident L.Expression
processExpressions = mconcat . map processExpression . HM.elems

processExpression :: K.Expression -> HM.HashMap Ident L.Expression
processExpression exp' =
    let reader = processExpression' exp'
        environment =
            ExpressionProcessorEnvironment
                { getExpressionProcessorEnvironmentBoundVariables = HS.empty
                , getExpressionProcessorEnvironmentScope = []
                , getExpressionProcessorEnvironmentSubstitution = HM.empty
                }
        writer = runReaderT reader environment
        state = execWriterT writer
     in evalState state 0

type ExpressionProcessor
     = ReaderT ExpressionProcessorEnvironment (WriterT (HM.HashMap Ident L.Expression) (State Int))

data ExpressionProcessorEnvironment = ExpressionProcessorEnvironment
    { getExpressionProcessorEnvironmentBoundVariables :: HS.HashSet Ident
    , getExpressionProcessorEnvironmentScope :: [Ident]
    , getExpressionProcessorEnvironmentSubstitution :: HM.HashMap Ident L.Exp
    }

bindVariables ::
       [WithLocation Ident]
    -> ExpressionProcessorEnvironment
    -> ExpressionProcessorEnvironment
bindVariables idents env
    | ExpressionProcessorEnvironment { getExpressionProcessorEnvironmentBoundVariables = bound
                                     , getExpressionProcessorEnvironmentSubstitution = substitution
                                     } <- env
    , vars <- map getValue idents =
        env
            { getExpressionProcessorEnvironmentBoundVariables =
                  bound <> HS.fromList vars
            , getExpressionProcessorEnvironmentSubstitution =
                  foldr HM.delete substitution vars
            }

addSubstitutions ::
       HM.HashMap Ident L.Exp
    -> ExpressionProcessorEnvironment
    -> ExpressionProcessorEnvironment
addSubstitutions subs env
    | ExpressionProcessorEnvironment { getExpressionProcessorEnvironmentBoundVariables = bound
                                     , getExpressionProcessorEnvironmentSubstitution = substitution
                                     } <- env =
        env
            { getExpressionProcessorEnvironmentBoundVariables =
                  foldr HS.delete bound (HM.keys subs)
            , getExpressionProcessorEnvironmentSubstitution =
                  substitution <> subs
            }

increaseScope ::
       Ident -> ExpressionProcessorEnvironment -> ExpressionProcessorEnvironment
increaseScope ident env =
    env
        { getExpressionProcessorEnvironmentScope =
              getExpressionProcessorEnvironmentScope env ++ [ident]
        }

generateNewIdent :: ExpressionProcessor (WithLocation Ident)
generateNewIdent =
    lift . lift $ do
        counter <- get
        let ident =
                withDummyLocation $ IdentGenerated IdentEnvironmentLet counter
        modify (+ 1)
        return ident

processExpression' :: K.Expression -> ExpressionProcessor ()
processExpression' K.Expression { K.getExpressionName = name
                                , K.getExpressionBody = body
                                , K.getExpressionType = type'
                                } = do
    let name' = getValue name
    newBody <- local (increaseScope name') $ processExp body
    scope <- asks getExpressionProcessorEnvironmentScope
    let newName =
            if null scope
                then name'
                else IdentScoped $ scope ++ [name']
        newExpression =
            L.Expression
                { L.getExpressionName = name $> newName
                , L.getExpressionBody = newBody
                , L.getExpressionType = type'
                }
    lift . tell $ HM.singleton (getValue name) newExpression

processExp :: WithLocation K.Exp -> ExpressionProcessor (WithLocation L.Exp)
processExp exp' =
    (exp' $>) <$>
    case getValue exp' of
        K.ExpVar name -> do
            substitution <- asks getExpressionProcessorEnvironmentSubstitution
            return $
                fromMaybe
                    (L.ExpVar name)
                    (HM.lookup (getValue name) substitution)
        K.ExpConstr name -> return $ L.ExpConstr name
        K.ExpConst c -> return $ L.ExpConst c
        K.ExpAbstraction var inner -> do
            innerProcessed <- local (bindVariables [var]) $ processExp inner
            return $ L.ExpAbstraction var innerProcessed
        K.ExpCase var type' params ifTrue ifFalse -> do
            ifTrueProcessed <- local (bindVariables params) $ processExp ifTrue
            return $ L.ExpCase var type' params ifTrueProcessed ifFalse
        K.ExpApplication func args ->
            liftM2 L.ExpApplication (processExp func) (mapM processExp args)
        K.ExpLet exps inner -> do
            newSubstitution <- processNestedExpressions exps
            (getValue <$>) . local (addSubstitutions newSubstitution) $
                processExp inner

processNestedExpressions ::
       HM.HashMap Ident K.Expression
    -> ExpressionProcessor (HM.HashMap Ident L.Exp)
processNestedExpressions expressions = do
    let definedIdents = HM.keysSet expressions
    nestedExpressions <-
        mapHashMapM (processNestedExpression definedIdents) expressions
    let newExpressions = HM.map fst nestedExpressions
        substitution = HM.map snd nestedExpressions
    _ <-
        mapHashMapM
            (local (addSubstitutions substitution) . processExpression')
            newExpressions
    return substitution

processNestedExpression ::
       HS.HashSet Ident
    -> K.Expression
    -> ExpressionProcessor (K.Expression, L.Exp)
processNestedExpression definedIdents K.Expression { K.getExpressionBody = body
                                                   , K.getExpressionType = type'
                                                   } = do
    boundVars <- asks getExpressionProcessorEnvironmentBoundVariables
    let stillBoundVars = boundVars `HS.difference` definedIdents
        freeVars = getFreeVariables (getValue body)
        shouldBind = stillBoundVars `HS.intersection` freeVars
        identsToBind = map withDummyLocation $ HS.toList shouldBind
    newName <- generateNewIdent
    let substitution = createSubstitution newName identsToBind
        newBody = createExpWithIdents identsToBind body
        newType = createSignature (length identsToBind) <$> type'
        newExpression =
            K.Expression
                { K.getExpressionName = newName
                , K.getExpressionBody = newBody
                , K.getExpressionType = newType
                }
    return (newExpression, substitution)

createSubstitution :: WithLocation Ident -> [WithLocation Ident] -> L.Exp
createSubstitution newName identsToBind =
    let lExp = L.ExpVar newName
        identsVars = map (withDummyLocation . L.ExpVar) identsToBind
     in case identsVars of
            [] -> lExp
            f:rest -> L.ExpApplication (withDummyLocation lExp) (f NE.:| rest)

createExpWithIdents ::
       [WithLocation Ident] -> WithLocation K.Exp -> WithLocation K.Exp
createExpWithIdents identsToBind exp' =
    let makeLambda ident res = withDummyLocation $ K.ExpAbstraction ident res
     in foldr makeLambda exp' identsToBind

createSignature :: Int -> TypeSignature -> TypeSignature
createSignature paramsCount signature
    | TypeSignature { getTypeSignatureKindParams = kindParams
                    , getTypeSignatureTypeParams = typeParams
                    } <- signature =
        let identsPositions = [0 .. paramsCount - 1]
            makeKindIdent = IdentGenerated IdentEnvironmentKindVariable
            makeTypeIdent = IdentGenerated IdentEnvironmentTypeVariable
            newKindParams =
                map (\i -> (makeKindIdent i, SortSquare)) identsPositions
            newTypeParams =
                map
                    (\i -> (makeTypeIdent i, KindVar $ makeKindIdent i))
                    identsPositions
         in signature
                { getTypeSignatureKindParams = newKindParams ++ kindParams
                , getTypeSignatureTypeParams = newTypeParams ++ typeParams
                }
