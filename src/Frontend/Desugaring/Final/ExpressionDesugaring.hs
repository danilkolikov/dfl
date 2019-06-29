{- |
Module      :  Frontend.Desugaring.Final.ExpressionDesugaring
Description :  Final desugaring of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of expressions and related nodes
-}
module Frontend.Desugaring.Final.ExpressionDesugaring where

import Control.Monad (liftM2)
import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.ExpressionDesugaringAssignment
import Frontend.Desugaring.Final.ExpressionDesugaringBase
import Frontend.Desugaring.Final.ExpressionDesugaringCase
    ( desugarCase
    , desugarPatternsToAbstraction
    )
import Frontend.Desugaring.Final.ExpressionDesugaringStmt
import qualified Frontend.Desugaring.Final.ResolvedAst as R
import Frontend.Desugaring.Final.Utils
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Desugar a list of assignments to Expressions
desugarAssignments ::
       [WithLocation R.Assignment] -> ExpressionDesugaringProcessor Expressions
desugarAssignments assignments =
    mapM desugarAssignment assignments >>= desugarPreparedAssignments

-- | Desugar a list of assignments to Methods
desugarMethods ::
       [WithLocation R.Assignment]
    -> ExpressionDesugaringProcessor Methods
desugarMethods assignments =
    mapM desugarAssignment assignments >>= desugarPreparedMethods


-- | Desugar a single assignment
desugarAssignment ::
       WithLocation R.Assignment
    -> ExpressionDesugaringProcessor (WithLocation PreparedAssignment)
desugarAssignment assignment =
    (assignment $>) <$>
    case getValue assignment of
        R.AssignmentType name context type' ->
            return $
            PreparedAssignmentType name (map desugarConstraint context) type'
        R.AssignmentName name pats exp' ->
            PreparedAssignmentName name pats <$> desugarExp exp'
        R.AssignmentPattern pat exp' ->
            PreparedAssignmentPattern pat <$> desugarExp exp'

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
            desugaredDecls <- desugarAssignments decls
            desugaredExp <- desugarExp exp'
            return $ ExpLet desugaredDecls desugaredExp
        R.ExpApplication func args ->
            liftM2 ExpApplication (desugarExp func) (mapM desugarExp args)
        R.ExpTyped exp' context type' -> do
            desugaredE <- desugarExp exp'
            newIdent <- generateNewIdent'
            let desugaredContext = map desugarConstraint context
                typeSignature = TypeSignature desugaredContext type'
                expression =
                    Expression newIdent desugaredE (Just typeSignature)
                expressions = HM.singleton (getValue newIdent) expression
                resExp = withDummyLocation $ ExpVar newIdent
            return $ ExpLet expressions resExp
        R.ExpLeftSection exp' op -> do
            desugaredExp <- desugarExp exp'
            desugaredOp <- desugarExp op
            newIdent <- generateNewIdent'
            let arg = withDummyLocation $ ExpVar newIdent
                application =
                    withDummyLocation $
                    ExpApplication desugaredOp (desugaredExp NE.:| [arg])
            return $ ExpAbstraction newIdent application
        R.ExpRightSection op exp' -> do
            desugaredOp <- desugarExp op
            desugaredExp <- desugarExp exp'
            newIdent <- generateNewIdent'
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
        R.StmtLet decls -> PreparedStmtLet <$> desugarAssignments decls

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
                (desugarAssignments decls)

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
