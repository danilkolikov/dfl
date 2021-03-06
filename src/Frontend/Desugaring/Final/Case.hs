{- |
Module      :  Frontend.Desugaring.Final.Case
Description :  Desugaring of case expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of case expressions
-}
module Frontend.Desugaring.Final.Case where

import Control.Monad (replicateM)
import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)

import Core.PredefinedIdents
import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.Base
import qualified Frontend.Desugaring.Record.Ast as R
import Frontend.Desugaring.Final.Util
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Desugar a case statement to an expression
desugarCase ::
       WithLocation Exp
    -> NE.NonEmpty (WithLocation PreparedAlt)
    -> ExpressionDesugaringProcessor Exp
desugarCase = desugarCase' Nothing

-- | Desugar a list of prepared alternatives to an abstraction
desugarAltsToAbstraction ::
       NE.NonEmpty (WithLocation PreparedAlt)
    -> ExpressionDesugaringProcessor (WithLocation Exp)
desugarAltsToAbstraction = desugarAltsToAbstraction' Nothing

-- | Desugar a non-empty list of patterns to an abstraction
desugarPatternsToAbstraction ::
       Int
    -> NE.NonEmpty (NE.NonEmpty (WithLocation R.Pattern), WithLocation Exp)
    -> ExpressionDesugaringProcessor Exp
desugarPatternsToAbstraction nPatterns patterns = do
    newIdents <- replicateM nPatterns generateNewIdent
    let tupleIdent = makeTuple nPatterns
        combinePattern (pat NE.:| rest) =
            case rest of
                [] -> pat
                _ -> withDummyLocation $ R.PatternConstr tupleIdent (pat : rest)
        makeAlt (pats, exp') =
            withDummyLocation $ PreparedAltSimple (combinePattern pats) exp'
        varExps@(firstVar NE.:| restVars) =
            fmap (withDummyLocation . ExpVar) (NE.fromList newIdents)
        combinedExpr =
            case restVars of
                [] -> firstVar
                _ ->
                    let tupleExp = withDummyLocation $ ExpVar tupleIdent
                     in withDummyLocation $ ExpApplication tupleExp varExps
    case' <- desugarCase' Nothing combinedExpr (fmap makeAlt patterns)
    let wrapToAbstraction ident exp' =
            ExpAbstraction ident (withDummyLocation exp')
        wrapped = foldr wrapToAbstraction case' newIdents
    return wrapped

-- | Desugar a case statement to an expression
desugarCase' ::
       Maybe (WithLocation Ident)
    -> WithLocation Exp
    -> NE.NonEmpty (WithLocation PreparedAlt)
    -> ExpressionDesugaringProcessor Exp
desugarCase' elseIdent exp' alts = do
    abstraction <- desugarAltsToAbstraction' elseIdent alts
    return $ ExpApplication abstraction (exp' NE.:| [])

-- | Desugar a list of prepared alternatives to an abstraction
desugarAltsToAbstraction' ::
       Maybe (WithLocation Ident)
    -> NE.NonEmpty (WithLocation PreparedAlt)
    -> ExpressionDesugaringProcessor (WithLocation Exp)
desugarAltsToAbstraction' elseIdent alts = do
    caseIdent <- generateNewIdent
    case' <- desugarAlts elseIdent caseIdent alts
    return . withDummyLocation $ ExpAbstraction caseIdent case'

-- | Desugar a list of alternatives
desugarAlts ::
       Maybe (WithLocation Ident)
    -> WithLocation Ident
    -> NE.NonEmpty (WithLocation PreparedAlt)
    -> ExpressionDesugaringProcessor (WithLocation Exp)
desugarAlts (Just elseIdent) caseIdent (first NE.:| []) =
    desugarAlt caseIdent elseIdent first
desugarAlts Nothing caseIdent (first NE.:| []) =
    abstractExpAway
        (\elseIdent -> desugarAlt caseIdent elseIdent first)
        undefinedExp
desugarAlts maybeElseIdent caseIdent (first NE.:| (s:rest)) = do
    elseExp <- desugarAlts maybeElseIdent caseIdent (s NE.:| rest)
    abstractExpAway (\elseIdent -> desugarAlt caseIdent elseIdent first) elseExp

-- | Desugar a single alternative
desugarAlt ::
       WithLocation Ident
    -> WithLocation Ident
    -> WithLocation PreparedAlt
    -> ExpressionDesugaringProcessor (WithLocation Exp)
desugarAlt caseIdent elseIdent prepared =
    case getValue prepared of
        PreparedAltSimple pat exp' ->
            desugarPattern caseIdent elseIdent exp' pat
        PreparedAltGuarded pat guardedExps decls -> do
            expression <- desugarGuardedExps guardedExps
            let result = expression $> wrapToLet decls expression
            desugarPattern caseIdent elseIdent result pat

-- | Desigar a list of guarded expressions to an expression
desugarGuardedExps ::
       NE.NonEmpty (WithLocation PreparedGuardedExp)
    -> ExpressionDesugaringProcessor (WithLocation Exp)
desugarGuardedExps (first NE.:| rest) = do
    desugaredRest <-
        case rest of
            [] -> return undefinedExp
            (s:others) -> desugarGuardedExps (s NE.:| others)
    let (PreparedGuardedExp stmts exp') = getValue first
    abstractExpAway
        (\elseIdent -> desugarGuards exp' elseIdent stmts)
        desugaredRest

-- | Desugar a list of guards to an expression
desugarGuards ::
       WithLocation Exp
    -> WithLocation Ident
    -> NE.NonEmpty (WithLocation PreparedStmt)
    -> ExpressionDesugaringProcessor (WithLocation Exp)
desugarGuards ifSuccess ifFail (first NE.:| rest) = do
    desugaredRest <-
        case rest of
            [] -> return ifSuccess
            (s:others) -> desugarGuards ifSuccess ifFail (s NE.:| others)
    desugarGuard desugaredRest ifFail first

-- | Desugar a single guard to an expression
desugarGuard ::
       WithLocation Exp
    -> WithLocation Ident
    -> WithLocation PreparedStmt
    -> ExpressionDesugaringProcessor (WithLocation Exp)
desugarGuard ifSuccess ifFail stmt =
    (stmt $>) <$>
    case getValue stmt of
        PreparedStmtPattern pat inner ->
            let altSuccess = withDummyLocation $ PreparedAltSimple pat ifSuccess
             in desugarCase' (Just ifFail) inner (altSuccess NE.:| [])
        PreparedStmtLet exps -> return $ wrapToLet exps ifSuccess
        PreparedStmtExp inner ->
            let truePattern = withDummyLocation $ R.PatternConstr trueIdent []
                altSuccess =
                    withDummyLocation $ PreparedAltSimple truePattern ifSuccess
             in desugarCase' (Just ifFail) inner (altSuccess NE.:| [])

-- | Desugar a single pattern to an expression
desugarPattern ::
       WithLocation Ident
    -> WithLocation Ident
    -> WithLocation Exp
    -> WithLocation R.Pattern
    -> ExpressionDesugaringProcessor (WithLocation Exp)
desugarPattern caseIdent elseIdent success patter =
    case getValue patter of
        R.PatternVar name pattern' ->
            let innerPattern =
                    fromMaybe (withDummyLocation R.PatternWildcard) pattern'
                abstraction = withDummyLocation $ ExpAbstraction name success
                var = withDummyLocation $ ExpVar caseIdent
                newSuccess =
                    withDummyLocation $
                    ExpApplication abstraction (var NE.:| [])
             in desugarPattern caseIdent elseIdent newSuccess innerPattern
        R.PatternWildcard -> return success
        R.PatternConst c -> do
            let application =
                    withDummyLocation $
                    ExpApplication
                        (makeExp eQUAL)
                        (withDummyLocation (ExpVar caseIdent) NE.:|
                         [withDummyLocation (ExpConst c)])
                makeCase newIdent =
                    return . withDummyLocation $
                    ExpCase newIdent trueIdent [] success elseIdent
            abstractExpAway makeCase application
        R.PatternConstr name args -> do
            newIdents <- mapM (const generateNewIdent) args
            let makeCases [] = return success
                makeCases ((innerIdent, innerPattern):rest) = do
                    res <- makeCases rest
                    desugarPattern innerIdent elseIdent res innerPattern
            cases <- makeCases (zip newIdents args)
            let case' = ExpCase caseIdent name newIdents cases elseIdent
            return . withDummyLocation $ case'

-- | Create an abstraction for the provided function, and apply it to the provided
-- | argument
abstractExpAway ::
       (WithLocation Ident -> ExpressionDesugaringProcessor (WithLocation Exp))
    -> WithLocation Exp
    -> ExpressionDesugaringProcessor (WithLocation Exp)
abstractExpAway func arg = do
    newIdent <- generateNewIdent
    body <- func newIdent
    let abstraction = withDummyLocation $ ExpAbstraction newIdent body
    return . withDummyLocation $ ExpApplication abstraction (arg NE.:| [])

-- | Possibly wrap list of expressions to a let statement
wrapToLet :: Expressions Exp -> WithLocation Exp -> Exp
wrapToLet decls exp' =
    if null decls
        then getValue exp'
        else ExpLet decls exp'
