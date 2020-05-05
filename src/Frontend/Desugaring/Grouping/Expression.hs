{- |
Module      :  Frontend.Desugaring.Grouping.Expression
Description :  Grouping of assignments
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Grouping of assignments and related nodes
-}
module Frontend.Desugaring.Grouping.Expression
    ( wrapExpression
    , GroupAssignments
    ) where

import Control.Monad (liftM2)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Functor (($>))

import Frontend.Desugaring.Grouping.Ast
import Frontend.Desugaring.Grouping.Base
import Frontend.Desugaring.Grouping.Util
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (WithLocation(..))

-- | A type of functions which group assignments
type GroupAssignments
     = [WithLocation I.Assignment] -> GroupingProcessor (Expressions Exp)

-- | A wrapper of expressions
type ExpressionWrapper = ReaderT GroupAssignments GroupingProcessor

-- | Wraps an expression
wrapExpression ::
       GroupAssignments
    -> WithLocation I.Exp
    -> GroupingProcessor (WithLocation Exp)
wrapExpression wrapper exp' = runReaderT (wrapExp exp') wrapper

wrapInfixExp ::
       WithLocation I.InfixExp -> ExpressionWrapper (WithLocation InfixExp)
wrapInfixExp iExp =
    (iExp $>) <$>
    case getValue iExp of
        I.InfixExpApplication l op r ->
            liftM2
                (\lE rE -> InfixExpApplication lE (wrapIdent op) rE)
                (wrapInfixExp l)
                (wrapInfixExp r)
        I.InfixExpNegated op inner ->
            InfixExpNegated (wrapIdent op) <$> wrapInfixExp inner
        I.InfixExpSimple inner -> InfixExpSimple <$> wrapExp inner

wrapExp :: WithLocation I.Exp -> ExpressionWrapper (WithLocation Exp)
wrapExp expr =
    (expr $>) <$>
    case getValue expr of
        I.ExpInfix inf -> ExpInfix <$> wrapInfixExp inf
        I.ExpTyped inner context type' ->
            (\res -> ExpTyped res (map wrapConstraint context) (wrapType type')) <$>
            wrapExp inner
        I.ExpAbstraction pats inner ->
            ExpAbstraction (fmap wrapPattern pats) <$> wrapExp inner
        I.ExpLet assignments inner -> do
            groupAssignments <- ask
            liftM2 ExpLet (lift $ groupAssignments assignments) (wrapExp inner)
        I.ExpCase inner alts ->
            liftM2 ExpCase (wrapExp inner) (mapM wrapAlt alts)
        I.ExpDo stmts inner ->
            liftM2 ExpDo (mapM wrapStmt stmts) (wrapExp inner)
        I.ExpApplication func args ->
            liftM2 ExpApplication (wrapExp func) (mapM wrapExp args)
        I.ExpVar name -> return $ ExpVar (wrapIdent name)
        I.ExpConstr name -> return $ ExpConstr (wrapIdent name)
        I.ExpConst c -> return $ ExpConst c
        I.ExpListCompr inner stmts ->
            liftM2 ExpListCompr (wrapExp inner) (mapM wrapStmt stmts)
        I.ExpLeftSection l op -> liftM2 ExpLeftSection (wrapExp l) (wrapExp op)
        I.ExpRightSection op r ->
            liftM2 ExpRightSection (wrapExp op) (wrapExp r)
        I.ExpRecordConstr name bindings ->
            ExpRecordConstr (wrapIdent name) <$> mapM wrapBinding bindings
        I.ExpRecordUpdate inner bindings ->
            liftM2 ExpRecordUpdate (wrapExp inner) (mapM wrapBinding bindings)

wrapStmt :: WithLocation I.Stmt -> ExpressionWrapper (WithLocation Stmt)
wrapStmt stmt =
    (stmt $>) <$>
    case getValue stmt of
        I.StmtPattern pat expr -> StmtPattern (wrapPattern pat) <$> wrapExp expr
        I.StmtLet assignments -> do
            groupAssignments <- ask
            StmtLet <$> lift (groupAssignments assignments)
        I.StmtExp expr -> StmtExp <$> wrapExp expr

wrapAlt :: WithLocation I.Alt -> ExpressionWrapper (WithLocation Alt)
wrapAlt alt =
    (alt $>) <$>
    case getValue alt of
        I.AltSimple pat expr -> AltSimple (wrapPattern pat) <$> wrapExp expr
        I.AltGuarded pat guards assignments -> do
            groupAssignments <- ask
            liftM2
                (AltGuarded (wrapPattern pat))
                (mapM wrapGuard guards)
                (lift $ groupAssignments assignments)

wrapBinding ::
       WithLocation I.Binding -> ExpressionWrapper (WithLocation Binding)
wrapBinding binding =
    (binding $>) <$>
    case getValue binding of
        I.Binding name expr -> Binding (wrapIdent name) <$> wrapExp expr

wrapGuard ::
       WithLocation I.GuardedExp -> ExpressionWrapper (WithLocation GuardedExp)
wrapGuard guarded =
    (guarded $>) <$>
    case getValue guarded of
        I.GuardedExp stmts expr ->
            liftM2 GuardedExp (mapM wrapStmt stmts) (wrapExp expr)
