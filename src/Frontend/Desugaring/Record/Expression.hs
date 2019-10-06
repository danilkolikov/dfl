{- |
Module      :  Frontend.Desugaring.Record.Expression
Description :  Desugaring expressions, containing records
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with code for desugaring record patterns, construction and update
-}
module Frontend.Desugaring.Record.Expression where

import Control.Monad (liftM2, liftM3)
import Control.Monad.Trans.Class (lift)
import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)

import qualified Frontend.Desugaring.Fixity.Ast as F
import Frontend.Desugaring.Record.Ast
import Frontend.Desugaring.Record.Base
import Frontend.Desugaring.Record.Util
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)
import Util.HashMap

-- | Desugar records in the pattern
desugarPattern ::
       WithLocation F.Pattern
    -> RecordDesugaringProcessor (WithLocation Pattern)
desugarPattern pat =
    (pat $>) <$>
    case getValue pat of
        F.PatternWildcard -> return PatternWildcard
        F.PatternConst c -> return $ PatternConst c
        F.PatternVar name maybePat ->
            PatternVar name <$> mapM desugarPattern maybePat
        F.PatternConstr name args ->
            PatternConstr name <$> mapM desugarPattern args
        F.PatternRecord name bindings -> do
            dataType <- findDataTypeByConstructor name
            constructor <- lift $ lookupConstructor name dataType
            let prepareBinding binding =
                    case getValue binding of
                        F.PatternBinding name' pattern' ->
                            (name', const pattern')
            resolvedArgs <-
                lift $
                resolveBindings
                    constructor
                    (const $ withDummyLocation F.PatternWildcard)
                    (map prepareBinding bindings)
            PatternConstr name <$> mapM desugarPattern resolvedArgs

-- | Desugars records in expressions
desugarExpressions ::
       Expressions F.Exp -> RecordDesugaringProcessor (Expressions Exp)
desugarExpressions = mapHashMapM (mapExpressionM desugarExp)

-- | Desugar records in the expression
desugarExp :: WithLocation F.Exp -> RecordDesugaringProcessor (WithLocation Exp)
desugarExp expression =
    (expression $>) <$>
    case getValue expression of
        F.ExpVar name -> return $ ExpVar name
        F.ExpConstr name -> return $ ExpConstr name
        F.ExpConst value -> return $ ExpConst value
        F.ExpTyped exp' context type' -> do
            desugaredExp <- desugarExp exp'
            return $ ExpTyped desugaredExp context type'
        F.ExpAbstraction pats exp' ->
            liftM2 ExpAbstraction (mapM desugarPattern pats) (desugarExp exp')
        F.ExpLet assignments exp' ->
            liftM2 ExpLet (desugarExpressions assignments) (desugarExp exp')
        F.ExpCase exp' alts ->
            liftM2 ExpCase (desugarExp exp') (mapM desugarAlt alts)
        F.ExpDo stmts exp' ->
            liftM2 ExpDo (mapM desugarStmt stmts) (desugarExp exp')
        F.ExpApplication func args ->
            liftM2 ExpApplication (desugarExp func) (mapM desugarExp args)
        F.ExpListCompr exp' stmts ->
            liftM2 ExpListCompr (desugarExp exp') (mapM desugarStmt stmts)
        F.ExpLeftSection exp' op ->
            liftM2 ExpLeftSection (desugarExp exp') (desugarExp op)
        F.ExpRightSection op exp' ->
            liftM2 ExpRightSection (desugarExp op) (desugarExp exp')
        F.ExpRecordConstr name bindings -> do
            dataType <- findDataTypeByConstructor name
            constructor <- lift $ lookupConstructor name dataType
            let prepareBinding binding =
                    case getValue binding of
                        F.Binding name' exp' -> do
                            desugaredExp <- desugarExp exp'
                            return (name', const desugaredExp)
            preparedBindings <- mapM prepareBinding bindings
            resolvedBindings <-
                lift $
                resolveBindings
                    constructor
                    (const undefinedExp)
                    preparedBindings
            let constr = ExpConstr name
            return $
                case resolvedBindings of
                    [] -> constr
                    (f:rest) -> ExpApplication (name $> constr) (f NE.:| rest)
        F.ExpRecordUpdate expToUpdate bindings -> do
            desugaredExp <- desugarExp expToUpdate
            let prepareBinding binding =
                    case getValue binding of
                        F.Binding name' exp' -> do
                            desugaredExp' <- desugarExp exp'
                            return (name', desugaredExp')
            prepared <- mapM prepareBinding . NE.toList $ bindings
            fieldsMap <- lift $ checkForDuplicateFields prepared
            let fieldsPairs = HM.toList fieldsMap
                fields = map snd fieldsPairs
                fieldNames = map fst fieldsPairs
            dataType <- findDataTypeByField $ head fields
            let constructors = map snd $ getDataTypeConstructors dataType
                requiredConstructors =
                    filterRequiredConstructors fieldNames constructors
            desugaredAlts <-
                lift $
                mapM (makeUpdateAlternative prepared) requiredConstructors
            nonEmptyAlts <-
                case desugaredAlts of
                    [] ->
                        raiseError $
                        RecordDesugaringErrorMissingConstructor fields
                    (s:rest) -> return $ s NE.:| rest
            return $ ExpCase desugaredExp nonEmptyAlts

-- | Desugar records in the alternative
desugarAlt :: WithLocation F.Alt -> RecordDesugaringProcessor (WithLocation Alt)
desugarAlt alt =
    (alt $>) <$>
    case getValue alt of
        F.AltSimple pat exp' ->
            liftM2 AltSimple (desugarPattern pat) (desugarExp exp')
        F.AltGuarded pat guardedExps assignments ->
            liftM3
                AltGuarded
                (desugarPattern pat)
                (mapM desugarGuardedExp guardedExps)
                (desugarExpressions assignments)

-- | Desugar records in the guarded expression
desugarGuardedExp ::
       WithLocation F.GuardedExp
    -> RecordDesugaringProcessor (WithLocation GuardedExp)
desugarGuardedExp ge =
    (ge $>) <$>
    case getValue ge of
        F.GuardedExp guards exp' ->
            liftM2 GuardedExp (mapM desugarStmt guards) (desugarExp exp')

-- | Desugar records in the statement
desugarStmt ::
       WithLocation F.Stmt -> RecordDesugaringProcessor (WithLocation Stmt)
desugarStmt stmt =
    (stmt $>) <$>
    case getValue stmt of
        F.StmtLet assignments -> StmtLet <$> desugarExpressions assignments
        F.StmtPattern pat exp' ->
            liftM2 StmtPattern (desugarPattern pat) (desugarExp exp')
        F.StmtExp exp' -> StmtExp <$> desugarExp exp'

-- | Check for duplicating fields in a pattern
checkForDuplicateFields ::
       [(WithLocation Ident, a)]
    -> RecordDesugaringExcept (HM.HashMap Ident (WithLocation Ident))
checkForDuplicateFields [] = return HM.empty
checkForDuplicateFields ((field, _):rest) = do
    fields <- checkForDuplicateFields rest
    case HM.lookup (getValue field) fields of
        Just field2 ->
            raiseRDError $ RecordDesugaringErrorDuplicateField field field2
        Nothing -> return $ HM.insert (getValue field) field fields

-- | Resolve field bindings
resolveBindings ::
       Constructor
    -> (Int -> a)
    -> [(WithLocation Ident, Int -> a)]
    -> RecordDesugaringExcept [a]
resolveBindings constructor ifNotFound bindings =
    checkForDuplicateFields bindings >> do
        let fields = getConstructorFields constructor
            getBindingPosition (field, res) = do
                position <-
                    case HM.lookup (getValue field) fields of
                        Just pos -> return pos
                        Nothing ->
                            raiseRDError $
                            RecordDesugaringErrorUnknownField field
                return (position, res position)
        positions <- mapM getBindingPosition bindings
        let positionToRes = HM.fromList positions
            resForPosition pos =
                fromMaybe (ifNotFound pos) (HM.lookup pos positionToRes)
            args = getConstructorArgs constructor
            res = map resForPosition [0 .. length args - 1]
        return res

-- | Find constructors which has all fields out of specified
filterRequiredConstructors :: [F.Ident] -> [Constructor] -> [Constructor]
filterRequiredConstructors fields =
    filter
        (\c -> all (\field -> HM.member field (getConstructorFields c)) fields)

-- | Create one alternative in the record update expression
makeUpdateAlternative ::
       [(WithLocation Ident, WithLocation Exp)]
    -> Constructor
    -> RecordDesugaringExcept (WithLocation Alt)
makeUpdateAlternative fields constructor = do
    let makeMissing pos =
            (makePattern pos, withDummyLocation . ExpVar $ makeIdent pos)
        makeField (name, exp') = (name, \p -> (makePattern p, exp'))
        preparedFields = map makeField fields
    resolvedBindings <- resolveBindings constructor makeMissing preparedFields
    let (idents, exps) = unzip resolvedBindings
        constr = getConstructorName constructor
        constrExp = withDummyLocation $ ExpConstr constr
        pattern' = withDummyLocation $ PatternConstr constr idents
        expr = withDummyLocation $ ExpApplication constrExp (NE.fromList exps)
    return . withDummyLocation $ AltSimple pattern' expr
