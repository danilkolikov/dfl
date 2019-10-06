{- |
Module      :  Frontend.Desugaring.Checking.Expression
Description :  Ambiguity checks of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Ambiguity checks of expressions
-}
module Frontend.Desugaring.Checking.Expression
    ( checkExpressions
    , checkMethods
    , checkExps
    ) where

import Control.Monad (foldM, liftM2, liftM3)
import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Checking.Base
import Frontend.Desugaring.Checking.Pattern
import Frontend.Desugaring.Checking.Util
import Frontend.Desugaring.Grouping.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Checks idents in an expression for ambiguity
checkExpression :: Expression Exp -> CheckingProcessor (Ident, Expression Exp)
checkExpression Expression { getExpressionName = name
                           , getExpressionType = type'
                           , getExpressionFixity = fixity
                           , getExpressionBody = body
                           } = do
    checkedName <- checkExpressionName name
    checkedType <- traverse checkTypeSignature type'
    checkedBody <- checkExp body
    let res =
            Expression
                { getExpressionName = checkedName
                , getExpressionType = checkedType
                , getExpressionFixity = fixity
                , getExpressionBody = checkedBody
                }
    return (getValue checkedName, res)

-- | Checks idents in expressions for ambiguity
checkExpressions :: Expressions Exp -> CheckingProcessor (Expressions Exp)
checkExpressions = (HM.fromList <$>) . mapM checkExpression . HM.elems

-- | Checks idents in a method for ambiguity
checkMethod :: Method Exp -> CheckingProcessor (Ident, Method Exp)
checkMethod Method { getMethodName = name
                   , getMethodType = type'
                   , getMethodFixity = fixity
                   , getMethodBody = body
                   } = do
    checkedName <- checkExpressionName name
    checkedType <- checkTypeSignature type'
    checkedBody <- traverse checkExp body
    let res =
            Method
                { getMethodName = checkedName
                , getMethodType = checkedType
                , getMethodFixity = fixity
                , getMethodBody = checkedBody
                }
    return (getValue checkedName, res)

-- | Checks idents in methods for ambiguity
checkMethods :: Methods Exp -> CheckingProcessor (Methods Exp)
checkMethods = (HM.fromList <$>) . mapM checkMethod . HM.elems

-- | Checks idents in raw expressions for ambiguity
checkExps :: Exps Exp -> CheckingProcessor (Exps Exp)
checkExps = (HM.fromList <$>) . mapM checkSingleExp . HM.toList
  where
    checkSingleExp (name, expr) = do
        checkedName <- checkExpressionName (withDummyLocation name)
        checkedExp <- checkExp expr
        return (getValue checkedName, checkedExp)

-- | Checks idents in a raw expression for ambiguity
checkExp :: WithLocation Exp -> CheckingProcessor (WithLocation Exp)
checkExp expr =
    (expr $>) <$>
    case getValue expr of
        ExpInfix inf -> ExpInfix <$> checkInfixExp inf
        ExpTyped inner context type' ->
            liftM3
                ExpTyped
                (checkExp inner)
                (mapM checkConstraint context)
                (checkType type')
        ExpAbstraction pats inner -> do
            checkedPats <- mapM checkPattern pats
            let vars =
                    HS.unions . map getPatternVariables $ NE.toList checkedPats
            checkedExp <- defineLocalNames vars $ checkExp inner
            return $ ExpAbstraction checkedPats checkedExp
        ExpLet assignments inner -> do
            let names = HM.keysSet assignments
            defineLocalNames names $
                liftM2 ExpLet (checkExpressions assignments) (checkExp inner)
        ExpCase inner alts ->
            liftM2 ExpCase (checkExp inner) (mapM checkAlt alts)
        ExpDo stmts inner -> checkStmts ExpDo stmts inner
        ExpApplication func args ->
            liftM2 ExpApplication (checkExp func) (mapM checkExp args)
        ExpVar name -> ExpVar <$> checkExpressionName name
        ExpConstr name -> ExpConstr <$> checkExpressionName name
        ExpConst c -> return $ ExpConst c
        ExpListCompr inner stmts ->
            checkStmts (flip ExpListCompr . NE.fromList) (NE.toList stmts) inner
        ExpLeftSection l op -> liftM2 ExpLeftSection (checkExp l) (checkExp op)
        ExpRightSection op r ->
            liftM2 ExpRightSection (checkExp op) (checkExp r)
        ExpRecordConstr name bindings ->
            liftM2
                ExpRecordConstr
                (checkExpressionName name)
                (mapM checkBinding bindings)
        ExpRecordUpdate inner bindings ->
            liftM2 ExpRecordUpdate (checkExp inner) (mapM checkBinding bindings)

-- | Checks idents in an infix expression for ambiguity
checkInfixExp ::
       WithLocation InfixExp -> CheckingProcessor (WithLocation InfixExp)
checkInfixExp infixExp =
    (infixExp $>) <$>
    case getValue infixExp of
        InfixExpApplication l op r ->
            liftM3
                InfixExpApplication
                (checkInfixExp l)
                (checkExpressionName op)
                (checkInfixExp r)
        InfixExpNegated op inner ->
            liftM2
                InfixExpNegated
                (checkExpressionName op)
                (checkInfixExp inner)
        InfixExpSimple inner -> InfixExpSimple <$> checkExp inner

-- | Checks idents in statements for ambiguity
checkStmts ::
       ([WithLocation Stmt] -> WithLocation Exp -> a)
    -> [WithLocation Stmt]
    -> WithLocation Exp
    -> CheckingProcessor a
checkStmts wrapper stmts expr = do
    (checkedStmts, locals) <- foldM checkStmt ([], HS.empty) stmts
    checkedExpr <- defineLocalNames locals $ checkExp expr
    return $ wrapper checkedStmts checkedExpr

-- | Checks idents in a statement for ambiguity
checkStmt ::
       ([WithLocation Stmt], HS.HashSet Ident)
    -> WithLocation Stmt
    -> CheckingProcessor ([WithLocation Stmt], HS.HashSet Ident)
checkStmt (prev, names) stmt =
    case getValue stmt of
        StmtPattern pat expr -> do
            checkedPattern <- checkPattern pat
            let vars = getPatternVariables checkedPattern
                allNames = names <> vars
            checkedExp <- defineLocalNames allNames $ checkExp expr
            let res = stmt $> StmtPattern checkedPattern checkedExp
            return (prev ++ [res], allNames)
        StmtLet assignments -> do
            let allNames = names <> HM.keysSet assignments
            checked <- defineLocalNames allNames $ checkExpressions assignments
            let res = stmt $> StmtLet checked
            return (prev ++ [res], allNames)
        StmtExp expr -> do
            checked <- defineLocalNames names $ checkExp expr
            let res = stmt $> StmtExp checked
            return (prev ++ [res], names)

-- | Checks idents in an alternative for ambiguity
checkAlt :: WithLocation Alt -> CheckingProcessor (WithLocation Alt)
checkAlt alt =
    (alt $>) <$>
    case getValue alt of
        AltSimple pat expr -> do
            checkedPattern <- checkPattern pat
            let vars = getPatternVariables checkedPattern
            checkedExp <- defineLocalNames vars $ checkExp expr
            return $ AltSimple checkedPattern checkedExp
        AltGuarded pat guards assignments -> do
            checkedPattern <- checkPattern pat
            let vars = getPatternVariables checkedPattern
                names = HM.keysSet assignments
            (checkedAssignments, checkedGuards) <-
                defineLocalNames (vars <> names) $ do
                    checkedAssignments <- checkExpressions assignments
                    checkedGuards <- mapM checkGuard guards
                    return (checkedAssignments, checkedGuards)
            return $ AltGuarded checkedPattern checkedGuards checkedAssignments

-- | Checks idents in a binding for ambiguity
checkBinding :: WithLocation Binding -> CheckingProcessor (WithLocation Binding)
checkBinding binding =
    (binding $>) <$>
    case getValue binding of
        Binding name expr ->
            liftM2 Binding (checkExpressionName name) (checkExp expr)

-- | Checks idents in a guard for ambiguity
checkGuard ::
       WithLocation GuardedExp -> CheckingProcessor (WithLocation GuardedExp)
checkGuard guarded =
    (guarded $>) <$>
    case getValue guarded of
        GuardedExp stmts expr ->
            checkStmts (GuardedExp . NE.fromList) (NE.toList stmts) expr
