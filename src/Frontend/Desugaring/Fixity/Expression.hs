{- |
Module      :  Frontend.Desugaring.Fixity.Expression
Description :  Resolution of fixity of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Fixity resolution of expressions in DFL
-}
module Frontend.Desugaring.Fixity.Expression
    ( resolveExp
    , collectSignatures
    ) where

import Control.Monad (foldM, liftM2)
import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Fixity.Ast
import Frontend.Desugaring.Fixity.Base
import Frontend.Desugaring.Fixity.Pattern
import Frontend.Desugaring.Fixity.Resolution
import qualified Frontend.Desugaring.Grouping.Ast as G
import Frontend.Syntax.Position (WithLocation(..))
import Util.HashMap

-- | Resovles a map of expressions
resolveExpressions :: Expressions G.Exp -> FixityResolver (Expressions Exp)
resolveExpressions exprs =
    withSignatures exprs $ mapHashMapM resolveExpression exprs

-- | Does an action with signatures from expressions
withSignatures :: Expressions a -> FixityResolver b -> FixityResolver b
withSignatures exprs = defineOperators (collectSignatures exprs)

-- | Collects fixity signatures of expressions
collectSignatures :: Expressions a -> InfixOperators
collectSignatures = HM.mapMaybe getExpressionFixity

-- | Resolves fixity of an expression
resolveExpression :: (MapExpression e) => e G.Exp -> FixityResolver (e Exp)
resolveExpression = mapExpressionM resolveExp

-- | Resolves fixity of a raw expression
resolveExp :: WithLocation G.Exp -> FixityResolver (WithLocation Exp)
resolveExp expr =
    (expr $>) <$>
    case getValue expr of
        G.ExpInfix inf -> resolveInfixExp inf
        G.ExpTyped inner context type' ->
            (\res -> ExpTyped res context type') <$> resolveExp inner
        G.ExpAbstraction pats inner ->
            liftM2 ExpAbstraction (mapM resolvePattern pats) (resolveExp inner)
        G.ExpLet assignments inner -> do
            resolvedAssignments <- resolveExpressions assignments
            resolvedInner <-
                withSignatures resolvedAssignments (resolveExp inner)
            return $ ExpLet resolvedAssignments resolvedInner
        G.ExpCase inner alts ->
            liftM2 ExpCase (resolveExp inner) (mapM resolveAlt alts)
        G.ExpDo stmts inner -> resolveStmts ExpDo stmts inner
        G.ExpApplication func args ->
            liftM2 ExpApplication (resolveExp func) (mapM resolveExp args)
        G.ExpVar name -> return $ ExpVar name
        G.ExpConstr name -> return $ ExpConstr name
        G.ExpConst c -> return $ ExpConst c
        G.ExpListCompr inner stmts ->
            resolveStmts
                (flip ExpListCompr . NE.fromList)
                (NE.toList stmts)
                inner
        G.ExpLeftSection l op ->
            liftM2 ExpLeftSection (resolveExp l) (resolveExp op)
        G.ExpRightSection op r ->
            liftM2 ExpRightSection (resolveExp op) (resolveExp r)
        G.ExpRecordConstr name bindings ->
            ExpRecordConstr name <$> mapM resolveBinding bindings
        G.ExpRecordUpdate inner bindings ->
            liftM2
                ExpRecordUpdate
                (resolveExp inner)
                (mapM resolveBinding bindings)

-- | Resolves fixity of an infix expression
resolveInfixExp :: WithLocation G.InfixExp -> FixityResolver Exp
resolveInfixExp infixExp = collectFlatInfix infixExp >>= resolveFixity
  where
    collectFlatInfix expr =
        case getValue expr of
            G.InfixExpSimple inner -> do
                resolved <- resolveExp inner
                return [FlatInfixExp resolved]
            G.InfixExpNegated op inner -> do
                innerCollected <- collectFlatInfix inner
                return $ FlatInfixOp op : innerCollected
            G.InfixExpApplication l op r -> do
                lCollected <- collectFlatInfix l
                rCollected <- collectFlatInfix r
                return $ lCollected ++ [FlatInfixOp op] ++ rCollected

resolveStmts ::
       ([WithLocation Stmt] -> WithLocation Exp -> a)
    -> [WithLocation G.Stmt]
    -> WithLocation G.Exp
    -> FixityResolver a
resolveStmts resolveper stmts expr = do
    (resolvedStmts, signatures) <- foldM resolveStmt ([], HM.empty) stmts
    resolvedExpr <- defineOperators signatures $ resolveExp expr
    return $ resolveper resolvedStmts resolvedExpr

resolveStmt ::
       ([WithLocation Stmt], InfixOperators)
    -> WithLocation G.Stmt
    -> FixityResolver ([WithLocation Stmt], InfixOperators)
resolveStmt (prev, ops) stmt =
    case getValue stmt of
        G.StmtPattern pat expr -> do
            resolvedPattern <- resolvePattern pat
            resolvedExp <- defineOperators ops $ resolveExp expr
            let res = stmt $> StmtPattern resolvedPattern resolvedExp
            return (prev ++ [res], ops)
        G.StmtLet assignments -> do
            resolved <- defineOperators ops $ resolveExpressions assignments
            let res = stmt $> StmtLet resolved
                resOps = collectSignatures resolved
            return (prev ++ [res], ops <> resOps)
        G.StmtExp expr -> do
            resolved <- defineOperators ops $ resolveExp expr
            let res = stmt $> StmtExp resolved
            return (prev ++ [res], ops)

resolveAlt :: WithLocation G.Alt -> FixityResolver (WithLocation Alt)
resolveAlt alt =
    (alt $>) <$>
    case getValue alt of
        G.AltSimple pat expr ->
            liftM2 AltSimple (resolvePattern pat) (resolveExp expr)
        G.AltGuarded pat guards assignments -> do
            resolvedPattern <- resolvePattern pat
            resolvedAssignments <- resolveExpressions assignments
            resolvedGuards <-
                withSignatures resolvedAssignments $ mapM resolveGuard guards
            return $
                AltGuarded resolvedPattern resolvedGuards resolvedAssignments

resolveBinding ::
       WithLocation G.Binding -> FixityResolver (WithLocation Binding)
resolveBinding binding =
    (binding $>) <$>
    case getValue binding of
        G.Binding name expr -> Binding name <$> resolveExp expr

resolveGuard ::
       WithLocation G.GuardedExp -> FixityResolver (WithLocation GuardedExp)
resolveGuard guarded =
    (guarded $>) <$>
    case getValue guarded of
        G.GuardedExp stmts expr ->
            resolveStmts (GuardedExp . NE.fromList) (NE.toList stmts) expr
