{- |
Module      :  Frontend.Desugaring.Final.ExpressionDesugaring
Description :  Final desugaring of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of expressions and related nodes
-}
module Frontend.Desugaring.Final.ExpressionDesugaring
    ( desugarExp
    ) where

import Control.Monad (liftM2)
import Data.Functor ((<$))
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import qualified Frontend.Desugaring.Final.AssignmentDesugaring as A
import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.PatternDesugaring
import Frontend.Desugaring.Final.Processor
import Frontend.Desugaring.Final.Util
import qualified Frontend.Desugaring.Final.ResolvedAst as R
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Desugar list of assignments to Expressions
desugarAssignments ::
       [WithLocation R.Assignment] -> DesugaringProcessor Expressions
desugarAssignments = A.desugarAssignments desugarExp

-- | Desugar single expression
desugarExp :: WithLocation R.Exp -> DesugaringProcessor (WithLocation Exp)
desugarExp e =
    case getValue e of
        (R.ExpVar name) -> return $ ExpVar name <$ e
        (R.ExpConstr name) -> return $ ExpConstr name <$ e
        (R.ExpConst c) -> return $ ExpConst c <$ e
        (R.ExpLet decls exp') -> do
            desugaredDecls <- desugarAssignments decls
            desugaredExp <- desugarExp exp'
            return $ ExpLet desugaredDecls desugaredExp <$ e
        (R.ExpApplication func args) ->
            (<$ e) <$>
            liftM2 ExpApplication (desugarExp func) (mapM desugarExp args)
        (R.ExpTyped exp' context type') -> do
            desugaredE <- desugarExp exp'
            newIdent <- generateNewIdent
            let newIdent' = withDummyLocation newIdent
                desugaredContext = map desugarConstraint context
                typeSignature = TypeSignature desugaredContext type'
                expression =
                    Expression newIdent' desugaredE (Just typeSignature)
                expressions = HM.singleton newIdent expression
                resExp = withDummyLocation $ ExpVar newIdent'
            return $ ExpLet expressions resExp <$ e
        (R.ExpLeftSection exp' op) -> do
            desugaredExp <- desugarExp exp'
            desugaredOp <- desugarExp op
            newIdent <- generateNewIdent'
            let arg = withDummyLocation $ ExpVar newIdent
                application =
                    withDummyLocation $
                    ExpApplication desugaredOp (desugaredExp NE.:| [arg])
            return $ ExpAbstraction newIdent application <$ e
        (R.ExpRightSection op exp') -> do
            desugaredOp <- desugarExp op
            desugaredExp <- desugarExp exp'
            newIdent <- generateNewIdent'
            let arg = withDummyLocation $ ExpVar newIdent
                application =
                    withDummyLocation $
                    ExpApplication desugaredOp (arg NE.:| [desugaredExp])
            return $ ExpAbstraction newIdent application <$ e
        (R.ExpCase exp' alts) -> do
            desugaredExp <- desugarExp exp'
            let desugarAlt alt =
                    case getValue alt of
                        R.AltSimple pat altExp -> do
                            desugaredExp' <- desugarExp altExp
                            return (PreparedAlt pat desugaredExp')
                        R.AltGuarded _ _ _ -> error "TODO: Implement this case"
            desugaredAlts <- mapM desugarAlt alts
            abstraction <- desugarAltsToAbstraction desugaredAlts
            return $ ExpApplication abstraction (desugaredExp NE.:| []) <$ e
        (R.ExpAbstraction patterns exp') -> do
            desugaredExp <- desugarExp exp'
            desugarPatternsToAbstraction
                (NE.length patterns)
                ((patterns, desugaredExp) NE.:| [])
        (R.ExpDo stmts res)
            | [] <- stmts -> desugarExp res
            | first:rest <- stmts ->
                case getValue first of
                    R.StmtExp exp' -> do
                        desugaredExp <- desugarExp exp'
                        desugaredRest <-
                            desugarExp (withDummyLocation $ R.ExpDo rest res)
                        let func = makeExp iGNORING_BIND_NAME
                        return $
                            ExpApplication
                                func
                                (desugaredExp NE.:| [desugaredRest]) <$
                            e
                    R.StmtLet decls -> do
                        desugaredDecls <- desugarAssignments decls
                        desugaredRest <-
                            desugarExp (withDummyLocation $ R.ExpDo rest res)
                        return $ ExpLet desugaredDecls desugaredRest <$ e
                    R.StmtPattern pat exp' -> do
                        newIdent <- generateNewIdent'
                        desugaredRest <-
                            desugarExp (withDummyLocation $ R.ExpDo rest res)
                        let altPat = PreparedAlt pat desugaredRest
                            failFunction = makeExp fAIL_NAME
                            failArg =
                                withDummyLocation .
                                ExpConst . withDummyLocation . ConstString $
                                "Pattern not matched"
                            failPat = withDummyLocation R.PatternWildcard
                            failExp =
                                withDummyLocation $
                                ExpApplication failFunction (failArg NE.:| [])
                            altFail = PreparedAlt failPat failExp
                        abstraction <-
                            desugarAltsToAbstraction (altPat NE.:| [altFail])
                        desugaredExp <- desugarExp exp'
                        let expression = Expression newIdent abstraction Nothing
                            decls = HM.singleton (getValue newIdent) expression
                            bindFunction = makeExp bIND_NAME
                            bindApplication =
                                withDummyLocation $
                                ExpApplication
                                    bindFunction
                                    (desugaredExp NE.:| [abstraction])
                        return $ ExpLet decls bindApplication <$ e
        (R.ExpListCompr exp' (first NE.:| rest)) -> do
            let emptyList = makeConstr lIST_NAME
            desugaredRest <-
                case rest of
                    [] -> do
                        let append = makeExp cOLON_NAME
                        desugaredExp <- desugarExp exp'
                        return . withDummyLocation $
                            ExpApplication
                                append
                                (desugaredExp NE.:| [emptyList])
                    (s:stmts) ->
                        desugarExp . withDummyLocation $
                        R.ExpListCompr exp' (s NE.:| stmts)
            case getValue first of
                R.StmtExp cond -> do
                    desugaredCond <- desugarExp cond
                    let truePat = makeRPattern tRUE_NAME
                        altRest = PreparedAlt truePat desugaredRest
                        elsePat = withDummyLocation R.PatternWildcard
                        altElse = PreparedAlt elsePat emptyList
                    abstraction <-
                        desugarAltsToAbstraction (altRest NE.:| [altElse])
                    return $
                        ExpApplication abstraction (desugaredCond NE.:| []) <$ e
                R.StmtLet decls -> do
                    desugaredDecls <- desugarAssignments decls
                    return $ ExpLet desugaredDecls desugaredRest <$ e
                R.StmtPattern pat patExp' -> do
                    newIdent <- generateNewIdent'
                    let altPat = PreparedAlt pat desugaredRest
                        failPat = withDummyLocation R.PatternWildcard
                        altFail = PreparedAlt failPat emptyList
                    abstraction <-
                        desugarAltsToAbstraction (altPat NE.:| [altFail])
                    desugaredExp <- desugarExp patExp'
                    let expression = Expression newIdent abstraction Nothing
                        decls = HM.singleton (getValue newIdent) expression
                        function = makeExp cONCAT_MAP_NAME
                        application =
                            withDummyLocation $
                            ExpApplication
                                function
                                (desugaredExp NE.:| [abstraction])
                    return $ ExpLet decls application <$ e

makeRPattern :: EntityName -> WithLocation R.Pattern
makeRPattern =
    withDummyLocation . (`R.PatternConstr` []) . withDummyLocation . IdentNamed
