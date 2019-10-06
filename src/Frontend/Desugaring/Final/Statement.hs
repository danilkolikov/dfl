{- |
Module      :  Frontend.Desugaring.Final.Statement
Description :  Desugaring of do statements and list comprehension
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of do statements and list comprehension
-}
module Frontend.Desugaring.Final.Statement where

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Core.PredefinedIdents
import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.Base
import Frontend.Desugaring.Final.Case
    ( desugarAltsToAbstraction
    , desugarCase
    , wrapToLet
    )
import qualified Frontend.Desugaring.Record.Ast as R
import Frontend.Desugaring.Final.Util
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Desugar a do statement
desugarDo ::
       [WithLocation PreparedStmt]
    -> WithLocation Exp
    -> ExpressionDesugaringProcessor Exp
desugarDo [] exp' = return $ getValue exp'
desugarDo (stmt:rest) exp' = do
    desugaredRest <- desugarDo rest exp'
    let desugaredRest' = withDummyLocation desugaredRest
    desugarDoStmt stmt desugaredRest'

-- | Desugar a single part of the do statement
desugarDoStmt ::
       WithLocation PreparedStmt
    -> WithLocation Exp
    -> ExpressionDesugaringProcessor Exp
desugarDoStmt stmt exp' =
    case getValue stmt of
        PreparedStmtExp inner -> do
            let func = makeExp iGNORING_BIND
                args = inner NE.:| [exp']
            return $ ExpApplication func args
        PreparedStmtLet decls -> return $ wrapToLet decls exp'
        PreparedStmtPattern pat inner -> do
            newIdent <- generateNewIdent
            let altPat = withDummyLocation $ PreparedAltSimple pat exp'
                failFunction = makeExp fAIL
                failArg =
                    withDummyLocation .
                    ExpConst . withDummyLocation . ConstString $
                    "Pattern not matched"
                failPat = withDummyLocation R.PatternWildcard
                failExp =
                    withDummyLocation $
                    ExpApplication failFunction (failArg NE.:| [])
                altFail = withDummyLocation $ PreparedAltSimple failPat failExp
            abstraction <-
                desugarAltsToAbstraction (altPat NE.:| [altFail])
            let expression =
                    Expression newIdent abstraction Nothing Nothing
                expressionFunc = withDummyLocation $ ExpVar newIdent
                decls = HM.singleton (getValue newIdent) expression
                bindFunction = makeExp bIND
                bindApplication =
                    withDummyLocation $
                    ExpApplication bindFunction (inner NE.:| [expressionFunc])
            return $ ExpLet decls bindApplication

-- | Desugar a list comprehension statement
desugarListComprehension ::
       WithLocation Exp
    -> [WithLocation PreparedStmt]
    -> ExpressionDesugaringProcessor Exp
desugarListComprehension exp' [] =
    let append = makeExp cOLON
        args = exp' NE.:| [emptyList]
     in return $ ExpApplication append args
desugarListComprehension exp' (qual:rest) = do
    desugaredRest <- desugarListComprehension exp' rest
    let desugaredRest' = withDummyLocation desugaredRest
    desugarListComprehensionStmt desugaredRest' qual

-- | Desugar a single part of the list comprehension statement
desugarListComprehensionStmt ::
       WithLocation Exp
    -> WithLocation PreparedStmt
    -> ExpressionDesugaringProcessor Exp
desugarListComprehensionStmt exp' qual =
    case getValue qual of
        PreparedStmtExp cond -> do
            let truePat = makeRPattern tRUE
                altRest = withDummyLocation $ PreparedAltSimple truePat exp'
                elsePat = withDummyLocation R.PatternWildcard
                altElse =
                    withDummyLocation $ PreparedAltSimple elsePat emptyList
            desugarCase cond (altRest NE.:| [altElse])
        PreparedStmtLet decls -> return $ wrapToLet decls exp'
        PreparedStmtPattern pat inner -> do
            newIdent <- generateNewIdent
            let altPat = withDummyLocation $ PreparedAltSimple pat exp'
                failPat = withDummyLocation R.PatternWildcard
                altFail =
                    withDummyLocation $ PreparedAltSimple failPat emptyList
            abstraction <- desugarAltsToAbstraction (altPat NE.:| [altFail])
            let expression = Expression newIdent abstraction Nothing Nothing
                expressionFunc = withDummyLocation $ ExpVar newIdent
                decls = HM.singleton (getValue newIdent) expression
                function = makeExp cONCAT_MAP
                application =
                    withDummyLocation $
                    ExpApplication function (expressionFunc NE.:| [inner])
            return $ ExpLet decls application
