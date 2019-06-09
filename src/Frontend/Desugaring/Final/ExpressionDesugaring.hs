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
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Desugar list of assignments to Expressions
desugarAssignments ::
       [WithLocation I.Assignment] -> DesugaringProcessor Expressions
desugarAssignments = A.desugarAssignments desugarExp

-- | Desugar single expression
desugarExp :: WithLocation I.Exp -> DesugaringProcessor (WithLocation Exp)
desugarExp e =
    case getValue e of
        (I.ExpVar name) -> return $ ExpVar name <$ e
        (I.ExpConstr name) -> return $ ExpConstr name <$ e
        (I.ExpConst c) -> return $ ExpConst c <$ e
        (I.ExpLet decls exp') -> do
            desugaredDecls <- desugarAssignments decls
            desugaredExp <- desugarExp exp'
            return $ ExpLet desugaredDecls desugaredExp <$ e
        (I.ExpApplication func args) ->
            (<$ e) <$>
            liftM2 ExpApplication (desugarExp func) (mapM desugarExp args)
        (I.ExpTyped exp' context type') -> do
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
        (I.ExpLeftSection exp' op) -> do
            desugaredExp <- desugarExp exp'
            desugaredOp <- desugarExp op
            newIdent <- generateNewIdent'
            let arg = withDummyLocation $ ExpVar newIdent
                application =
                    withDummyLocation $
                    ExpApplication desugaredOp (desugaredExp NE.:| [arg])
            return $ ExpAbstraction newIdent application <$ e
        (I.ExpRightSection op exp') -> do
            desugaredOp <- desugarExp op
            desugaredExp <- desugarExp exp'
            newIdent <- generateNewIdent'
            let arg = withDummyLocation $ ExpVar newIdent
                application =
                    withDummyLocation $
                    ExpApplication desugaredOp (arg NE.:| [desugaredExp])
            return $ ExpAbstraction newIdent application <$ e
        (I.ExpCase exp' alts) -> do
            desugaredExp <- desugarExp exp'
            let desugarAlt alt =
                    case getValue alt of
                        I.AltSimple pat altExp -> do
                            desugaredExp' <- desugarExp altExp
                            return (PreparedAlt pat desugaredExp')
                        I.AltGuarded _ _ _ -> error "TODO: Implement this case"
            desugaredAlts <- mapM desugarAlt alts
            abstraction <- desugarAltsToAbstraction desugaredAlts
            return $ ExpApplication abstraction (desugaredExp NE.:| []) <$ e
        (I.ExpAbstraction patterns exp') -> do
            desugaredExp <- desugarExp exp'
            desugarPatternsToAbstraction
                (NE.length patterns)
                ((patterns, desugaredExp) NE.:| [])
        (I.ExpDo stmts res)
            | [] <- stmts -> desugarExp res
            | first:rest <- stmts ->
                case getValue first of
                    I.StmtExp exp' -> do
                        desugaredExp <- desugarExp exp'
                        desugaredRest <-
                            desugarExp (withDummyLocation $ I.ExpDo rest res)
                        let func = makeExp iGNORING_BIND_NAME
                        return $
                            ExpApplication
                                func
                                (desugaredExp NE.:| [desugaredRest]) <$
                            e
                    I.StmtLet decls -> do
                        desugaredDecls <- desugarAssignments decls
                        desugaredRest <-
                            desugarExp (withDummyLocation $ I.ExpDo rest res)
                        return $ ExpLet desugaredDecls desugaredRest <$ e
                    I.StmtPattern pat exp' -> do
                        newIdent <- generateNewIdent'
                        desugaredRest <-
                            desugarExp (withDummyLocation $ I.ExpDo rest res)
                        let altPat = PreparedAlt pat desugaredRest
                            failFunction = makeExp fAIL_NAME
                            failArg =
                                withDummyLocation .
                                ExpConst . withDummyLocation . ConstString $
                                "Pattern not matched"
                            failPat = withDummyLocation I.PatternWildcard
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
        (I.ExpListCompr exp' (first NE.:| rest)) -> do
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
                        I.ExpListCompr exp' (s NE.:| stmts)
            case getValue first of
                I.StmtExp cond -> do
                    desugaredCond <- desugarExp cond
                    let truePat = makeIPattern tRUE_NAME
                        altRest = PreparedAlt truePat desugaredRest
                        elsePat = withDummyLocation I.PatternWildcard
                        altElse = PreparedAlt elsePat emptyList
                    abstraction <-
                        desugarAltsToAbstraction (altRest NE.:| [altElse])
                    return $
                        ExpApplication abstraction (desugaredCond NE.:| []) <$ e
                I.StmtLet decls -> do
                    desugaredDecls <- desugarAssignments decls
                    return $ ExpLet desugaredDecls desugaredRest <$ e
                I.StmtPattern pat patExp' -> do
                    newIdent <- generateNewIdent'
                    let altPat = PreparedAlt pat desugaredRest
                        failPat = withDummyLocation I.PatternWildcard
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
        (I.ExpRecordConstr name bindings) -> do
            dataType <- findDataTypeByConstructor name
            constructor <- lookupConstructor name dataType
            let prepareBinding binding =
                    case getValue binding of
                        I.Binding name' exp' -> (name', desugarExp exp')
            expResolver <-
                resolveBindings
                    constructor
                    (return undefinedExp)
                    (map prepareBinding bindings)
            args <- sequence expResolver
            let constr = ExpConstr name <$ name
                application =
                    case args of
                        [] -> constr
                        (f:rest) -> ExpApplication constr (f NE.:| rest) <$ e
            return application
        (I.ExpRecordUpdate expToUpdate bindings) -> do
            let prepareBinding binding =
                    case getValue binding of
                        I.Binding name' exp' -> (name', exp')
                prepared = map prepareBinding . NE.toList $ bindings
            fieldsMap <- checkForDuplicates prepared
            let fieldsPairs = HM.toList fieldsMap
                fields = map snd fieldsPairs
                fieldNames = map fst fieldsPairs
            dataType <- findDataTypeByField $ head fields
            let constructors = getConstructors dataType
                requiredConstructors =
                    filter
                        (\c ->
                             all
                                 (\field ->
                                      HM.member field (getConstructorFields c))
                                 fieldNames)
                        constructors
                prepareConstructor c = do
                    let makeMissing = do
                            ident <- generateNewIdent'
                            return (ident, withDummyLocation $ ExpVar ident)
                        makeField (name, exp') =
                            ( name
                            , do ident <- generateNewIdent'
                                 desugared <- desugarExp exp'
                                 return (ident, desugared))
                    resolved <-
                        resolveBindings c makeMissing (map makeField prepared)
                    pairs <- sequence resolved
                    let (idents, exps) = unzip pairs
                        constr = getConstructorName c
                        constrExp = withDummyLocation $ ExpConstr constr
                        pattern' =
                            withDummyLocation $ PatternConstr constr idents
                        expr =
                            withDummyLocation $
                            ExpApplication constrExp (NE.fromList exps)
                    return (pattern', expr)
            desugaredExpr <- desugarExp expToUpdate
            desugaredCases <- mapM prepareConstructor requiredConstructors
            nonEmptyCases <-
                case desugaredCases of
                    [] -> raiseError $ DesugaringErrorMissingConstructor fields
                    (s:rest) -> return $ s NE.:| rest
            let prepareCase ident elseIdent (pattern', expr) =
                    return . withDummyLocation $
                    ExpCase ident pattern' expr elseIdent
            abstraction <- desugarCasesToAbstraction prepareCase nonEmptyCases
            return $ ExpApplication abstraction (desugaredExpr NE.:| []) <$ e

makeIPattern :: EntityName -> WithLocation I.Pattern
makeIPattern =
    withDummyLocation . (`I.PatternConstr` []) . withDummyLocation . IdentNamed
