{- |
Module      :  Frontend.Desugaring.Final.Expression
Description :  Final desugaring of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of expressions and related nodes
-}
module Frontend.Desugaring.Final.Expression where

import Control.Monad (liftM2)
import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.Base
import Frontend.Desugaring.Final.Case
    ( desugarCase
    , desugarPatternsToAbstraction
    )
import Frontend.Desugaring.Final.Statement
import qualified Frontend.Desugaring.Record.Ast as R
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)
import Util.HashMap

-- | Desugars a map of expressions or methods
desugarExpressions ::
       (MapExpression e)
    => HM.HashMap Ident (e R.Exp)
    -> ExpressionDesugaringProcessor (HM.HashMap Ident (e Exp))
desugarExpressions = mapHashMapM (mapExpressionM desugarExp)

-- | Desugar a single expression
desugarExp ::
       WithLocation R.Exp -> ExpressionDesugaringProcessor (WithLocation Exp)
desugarExp e =
    (e $>) <$>
    case getValue e of
        R.ExpVar name -> return $ ExpVar name
        R.ExpConstr name -> return $ ExpConstr name
        R.ExpConst c -> return $ ExpConst c
        R.ExpLet decls exp' -> do
            desugaredDecls <- desugarExpressions decls
            desugaredExp <- desugarExp exp'
            return $ ExpLet desugaredDecls desugaredExp
        R.ExpApplication func args ->
            liftM2 ExpApplication (desugarExp func) (mapM desugarExp args)
        R.ExpTyped exp' context type' -> do
            desugaredE <- desugarExp exp'
            newIdent <- generateNewIdent
            let typeSignature = TypeSignature context type'
                expression =
                    Expression newIdent desugaredE (Just typeSignature) Nothing
                expressions = HM.singleton (getValue newIdent) expression
                resExp = withDummyLocation $ ExpVar newIdent
            return $ ExpLet expressions resExp
        R.ExpLeftSection exp' op -> do
            desugaredExp <- desugarExp exp'
            desugaredOp <- desugarExp op
            newIdent <- generateNewIdent
            let arg = withDummyLocation $ ExpVar newIdent
                application =
                    withDummyLocation $
                    ExpApplication desugaredOp (desugaredExp NE.:| [arg])
            return $ ExpAbstraction newIdent application
        R.ExpRightSection op exp' -> do
            desugaredOp <- desugarExp op
            desugaredExp <- desugarExp exp'
            newIdent <- generateNewIdent
            let arg = withDummyLocation $ ExpVar newIdent
                application =
                    withDummyLocation $
                    ExpApplication desugaredOp (arg NE.:| [desugaredExp])
            return $ ExpAbstraction newIdent application
        R.ExpCase exp' alts -> do
            desugaredExp <- desugarExp exp'
            desugaredAlts <- mapM desugarAlt alts
            desugarCase desugaredExp desugaredAlts
        R.ExpAbstraction patterns exp' -> do
            desugaredExp <- desugarExp exp'
            desugarPatternsToAbstraction
                (NE.length patterns)
                ((patterns, desugaredExp) NE.:| [])
        R.ExpDo stmts res -> do
            desugaredStmts <- mapM desugarStmt stmts
            desugaredRes <- desugarExp res
            desugarDo desugaredStmts desugaredRes
        R.ExpListCompr exp' stmts -> do
            desugaredExp <- desugarExp exp'
            desugaredStmts <- mapM desugarStmt stmts
            desugarListComprehension desugaredExp (NE.toList desugaredStmts)

-- | Desugar a single Stmt
desugarStmt ::
       WithLocation R.Stmt
    -> ExpressionDesugaringProcessor (WithLocation PreparedStmt)
desugarStmt stmt =
    (stmt $>) <$>
    case getValue stmt of
        R.StmtExp exp' -> PreparedStmtExp <$> desugarExp exp'
        R.StmtPattern pat exp' -> PreparedStmtPattern pat <$> desugarExp exp'
        R.StmtLet decls -> PreparedStmtLet <$> desugarExpressions decls

-- | Desugar a single Alt
desugarAlt ::
       WithLocation R.Alt
    -> ExpressionDesugaringProcessor (WithLocation PreparedAlt)
desugarAlt alt =
    (alt $>) <$>
    case getValue alt of
        R.AltSimple pat exp' -> PreparedAltSimple pat <$> desugarExp exp'
        R.AltGuarded pat guarded decls ->
            liftM2
                (PreparedAltGuarded pat)
                (mapM desugarGuardedExp guarded)
                (desugarExpressions decls)

-- | Desugar a single GuardedExp
desugarGuardedExp ::
       WithLocation R.GuardedExp
    -> ExpressionDesugaringProcessor (WithLocation PreparedGuardedExp)
desugarGuardedExp guarded =
    (guarded $>) <$>
    case getValue guarded of
        R.GuardedExp guards exp' ->
            liftM2
                PreparedGuardedExp
                (mapM desugarStmt guards)
                (desugarExp exp')
