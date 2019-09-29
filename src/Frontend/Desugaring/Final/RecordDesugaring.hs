{- |
Module      :  Frontend.Desugaring.Final.RecordDesugaring
Description :  Desugaring of records
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with code for desugaring of record patterns, construction and update
-}
module Frontend.Desugaring.Final.RecordDesugaring where

import Control.Monad (liftM2, liftM3)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromMaybe)

import Frontend.Desugaring.Final.Ast (Constructor(..), DataType(..), DataTypes)
import Frontend.Desugaring.Final.ResolvedAst
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.EntityName (uNDEFINED_NAME)
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Errors which may happen during desugaring of records
data RecordDesugaringError
    = RecordDesugaringErrorUnknownField (WithLocation Ident) -- ^ Unknown field
    | RecordDesugaringErrorUnknownConstructor (WithLocation Ident) -- ^ Unknown constructor
    | RecordDesugaringErrorDuplicateField (WithLocation Ident)
                                          (WithLocation Ident) -- ^ Some field is used twice in a binding
    | RecordDesugaringErrorMissingConstructor [WithLocation Ident] -- ^ No constructors including these fields
    deriving (Show, Eq)

-- | Context of record desugaring
data RecordDesugaringContext = RecordDesugaringContext
    { getFieldToTypeMap :: DataTypes
    , getConstructorToTypeMap :: DataTypes
    }

-- | Processor of computations which can throw RecordDesugaringError
type RecordDesugaringExcept a = Except RecordDesugaringError a

-- | Run record desugaring except
runRecordDesugaringExcept ::
       RecordDesugaringExcept a -> Either RecordDesugaringError a
runRecordDesugaringExcept = runExcept

-- | Processor of record desugaring
type RecordDesugaringProcessor a
     = ReaderT RecordDesugaringContext (Except RecordDesugaringError) a

-- | Run a record desugaring processor
runRecordDesugaringProcessor ::
       RecordDesugaringProcessor a
    -> DataTypes
    -> DataTypes
    -> Either RecordDesugaringError a
runRecordDesugaringProcessor rdp fieldToType constrToType =
    runExcept
        (runReaderT
             rdp
             RecordDesugaringContext
                 { getFieldToTypeMap = fieldToType
                 , getConstructorToTypeMap = constrToType
                 })

-- | Desugar records in the assignment
desugarAssignment ::
       WithLocation I.Assignment
    -> RecordDesugaringProcessor (WithLocation Assignment)
desugarAssignment assignment =
    (assignment $>) <$>
    case getValue assignment of
        I.AssignmentType name context type' ->
            return $ AssignmentType name context type'
        I.AssignmentFixity name fixity prec ->
            return $ AssignmentFixity name fixity prec
        I.AssignmentPattern pat exp' ->
            liftM2 AssignmentPattern (desugarPattern pat) (desugarExp exp')
        I.AssignmentName name patterns exp' ->
            liftM2
                (AssignmentName name)
                (mapM desugarPattern patterns)
                (desugarExp exp')

-- | Desugar records in the class assignment
desugarClassAssignment ::
       WithLocation I.ClassAssignment
    -> RecordDesugaringProcessor (WithLocation ClassAssignment)
desugarClassAssignment assignment =
    (assignment $>) <$>
    case getValue assignment of
        I.ClassAssignmentType name context type' ->
            return $ ClassAssignmentType name context type'
        I.ClassAssignmentFixity name fixity prec ->
            return $ ClassAssignmentFixity name fixity prec
        I.ClassAssignmentName name patterns exp' ->
            liftM2
                (ClassAssignmentName name)
                (mapM desugarPattern patterns)
                (desugarExp exp')

-- | Desugar records in the instance assignment
desugarInstAssignment ::
       WithLocation I.InstAssignment
    -> RecordDesugaringProcessor (WithLocation InstAssignment)
desugarInstAssignment assignment =
    (assignment $>) <$>
    case getValue assignment of
        I.InstAssignmentName name patterns exp' ->
            liftM2
                (InstAssignmentName name)
                (mapM desugarPattern patterns)
                (desugarExp exp')

-- | Desugar records in the pattern
desugarPattern ::
       WithLocation I.Pattern
    -> RecordDesugaringProcessor (WithLocation Pattern)
desugarPattern pat =
    (pat $>) <$>
    case getValue pat of
        I.PatternWildcard -> return PatternWildcard
        I.PatternConst c -> return $ PatternConst c
        I.PatternVar name maybePat ->
            PatternVar name <$> mapM desugarPattern maybePat
        I.PatternConstr name args ->
            PatternConstr name <$> mapM desugarPattern args
        I.PatternRecord name bindings -> do
            dataType <- findDataTypeByConstructor name
            constructor <- lift $ lookupConstructor name dataType
            let prepareBinding binding =
                    case getValue binding of
                        I.PatternBinding name' pattern' ->
                            (name', const pattern')
            resolvedArgs <-
                lift $
                resolveBindings
                    constructor
                    (const $ withDummyLocation I.PatternWildcard)
                    (map prepareBinding bindings)
            PatternConstr name <$> mapM desugarPattern resolvedArgs

-- | Desugar records in the expression
desugarExp :: WithLocation I.Exp -> RecordDesugaringProcessor (WithLocation Exp)
desugarExp expression =
    (expression $>) <$>
    case getValue expression of
        I.ExpVar name -> return $ ExpVar name
        I.ExpConstr name -> return $ ExpConstr name
        I.ExpConst value -> return $ ExpConst value
        I.ExpTyped exp' context type' -> do
            desugaredExp <- desugarExp exp'
            return $ ExpTyped desugaredExp context type'
        I.ExpAbstraction pats exp' ->
            liftM2 ExpAbstraction (mapM desugarPattern pats) (desugarExp exp')
        I.ExpLet assignments exp' ->
            liftM2 ExpLet (mapM desugarAssignment assignments) (desugarExp exp')
        I.ExpCase exp' alts ->
            liftM2 ExpCase (desugarExp exp') (mapM desugarAlt alts)
        I.ExpDo stmts exp' ->
            liftM2 ExpDo (mapM desugarStmt stmts) (desugarExp exp')
        I.ExpApplication func args ->
            liftM2 ExpApplication (desugarExp func) (mapM desugarExp args)
        I.ExpListCompr exp' stmts ->
            liftM2 ExpListCompr (desugarExp exp') (mapM desugarStmt stmts)
        I.ExpLeftSection exp' op ->
            liftM2 ExpLeftSection (desugarExp exp') (desugarExp op)
        I.ExpRightSection op exp' ->
            liftM2 ExpRightSection (desugarExp op) (desugarExp exp')
        I.ExpRecordConstr name bindings -> do
            dataType <- findDataTypeByConstructor name
            constructor <- lift $ lookupConstructor name dataType
            let prepareBinding binding =
                    case getValue binding of
                        I.Binding name' exp' -> do
                            desugaredExp <- desugarExp exp'
                            return (name', const desugaredExp)
                undefinedExp =
                    withDummyLocation . ExpVar . withDummyLocation . IdentNamed $
                    uNDEFINED_NAME
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
        I.ExpRecordUpdate expToUpdate bindings -> do
            desugaredExp <- desugarExp expToUpdate
            let prepareBinding binding =
                    case getValue binding of
                        I.Binding name' exp' -> do
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
desugarAlt :: WithLocation I.Alt -> RecordDesugaringProcessor (WithLocation Alt)
desugarAlt alt =
    (alt $>) <$>
    case getValue alt of
        I.AltSimple pat exp' ->
            liftM2 AltSimple (desugarPattern pat) (desugarExp exp')
        I.AltGuarded pat guardedExps assignments ->
            liftM3
                AltGuarded
                (desugarPattern pat)
                (mapM desugarGuardedExp guardedExps)
                (mapM desugarAssignment assignments)

-- | Desugar records in the guarded expression
desugarGuardedExp ::
       WithLocation I.GuardedExp
    -> RecordDesugaringProcessor (WithLocation GuardedExp)
desugarGuardedExp ge =
    (ge $>) <$>
    case getValue ge of
        I.GuardedExp guards exp' ->
            liftM2 GuardedExp (mapM desugarStmt guards) (desugarExp exp')

-- | Desugar records in the statement
desugarStmt ::
       WithLocation I.Stmt -> RecordDesugaringProcessor (WithLocation Stmt)
desugarStmt stmt =
    (stmt $>) <$>
    case getValue stmt of
        I.StmtLet assignments -> StmtLet <$> mapM desugarAssignment assignments
        I.StmtPattern pat exp' ->
            liftM2 StmtPattern (desugarPattern pat) (desugarExp exp')
        I.StmtExp exp' -> StmtExp <$> desugarExp exp'

-- | Function raises a RecordDesugaringError
raiseError :: RecordDesugaringError -> RecordDesugaringProcessor a
raiseError = lift . throwE

-- | Function finds a data type by a specified field
findDataTypeByField :: WithLocation Ident -> RecordDesugaringProcessor DataType
findDataTypeByField name = do
    context <- ask
    let fields = getFieldToTypeMap context
    case HM.lookup (getValue name) fields of
        Just dataType -> return dataType
        Nothing -> raiseError $ RecordDesugaringErrorUnknownField name

-- | Function finds a data type by a specified constructor
findDataTypeByConstructor ::
       WithLocation Ident -> RecordDesugaringProcessor DataType
findDataTypeByConstructor name = do
    context <- ask
    let constructors = getConstructorToTypeMap context
    case HM.lookup (getValue name) constructors of
        Just dataType -> return dataType
        Nothing -> raiseError $ RecordDesugaringErrorUnknownConstructor name

-- | Find a specific constructor in the data type or raise an error
lookupConstructor ::
       WithLocation Ident -> DataType -> RecordDesugaringExcept Constructor
lookupConstructor name dataType =
    case lookup (getValue name) (getDataTypeConstructors dataType) of
        Just c -> return c
        Nothing -> throwE $ RecordDesugaringErrorUnknownConstructor name

-- | Check for duplicating fields in a pattern
checkForDuplicateFields ::
       [(WithLocation Ident, a)]
    -> RecordDesugaringExcept (HM.HashMap Ident (WithLocation Ident))
checkForDuplicateFields [] = return HM.empty
checkForDuplicateFields ((field, _):rest) = do
    fields <- checkForDuplicateFields rest
    case HM.lookup (getValue field) fields of
        Just field2 -> throwE $ RecordDesugaringErrorDuplicateField field field2
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
                            throwE $ RecordDesugaringErrorUnknownField field
                return (position, res position)
        positions <- mapM getBindingPosition bindings
        let positionToRes = HM.fromList positions
            resForPosition pos =
                fromMaybe (ifNotFound pos) (HM.lookup pos positionToRes)
            args = getConstructorArgs constructor
            res = map resForPosition [0 .. length args - 1]
        return res

-- | Find constructors which has all fields out of specified
filterRequiredConstructors :: [I.Ident] -> [Constructor] -> [Constructor]
filterRequiredConstructors fields =
    filter
        (\c -> all (\field -> HM.member field (getConstructorFields c)) fields)

-- | Make an ident
makeIdent :: Int -> WithLocation I.Ident
makeIdent =
    withDummyLocation . I.IdentGenerated I.IdentEnvironmentRecordDesugaring

-- | Make a pattern
makePattern :: Int -> WithLocation Pattern
makePattern = withDummyLocation . (`PatternVar` Nothing) . makeIdent

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

-- | Make field getters for a data type
makeFieldGetters ::
       DataType -> Either RecordDesugaringError [WithLocation Assignment]
makeFieldGetters dataType =
    let constructors = map snd $ getDataTypeConstructors dataType
        fields =
            HS.toList . HS.unions . map (HM.keysSet . getConstructorFields) $
            constructors
     in runRecordDesugaringExcept $ mapM (makeFieldGetter constructors) fields

-- | Make a single field getter for a data type
makeFieldGetter ::
       [Constructor]
    -> Ident
    -> RecordDesugaringExcept (WithLocation Assignment)
makeFieldGetter constructors field = do
    let requiredConstructors = filterRequiredConstructors [field] constructors
    alternatives <-
        mapM
            (makeGetterAlternative (withDummyLocation field))
            requiredConstructors
    let newIdent = makeIdent (-1) -- Hacky way to get a new identifier
        newExp = withDummyLocation . ExpVar $ newIdent
        newPattern = withDummyLocation $ PatternVar newIdent Nothing
        caseExp = withDummyLocation $ ExpCase newExp (NE.fromList alternatives)
        abstraction =
            withDummyLocation $ ExpAbstraction (newPattern NE.:| []) caseExp
        fieldPattern =
            withDummyLocation $ PatternVar (withDummyLocation field) Nothing
        assignment =
            withDummyLocation $ AssignmentPattern fieldPattern abstraction
    return assignment

-- | Create one alternative in the field getter
makeGetterAlternative ::
       WithLocation Ident
    -> Constructor
    -> RecordDesugaringExcept (WithLocation Alt)
makeGetterAlternative field constructor = do
    let makeMissing _ = (withDummyLocation PatternWildcard, Nothing)
        preparedField =
            ( field
            , \p ->
                  ( makePattern p
                  , Just . withDummyLocation . ExpVar $ makeIdent p))
    resolvedBindings <- resolveBindings constructor makeMissing [preparedField]
    let (idents, exps) = unzip resolvedBindings
        constr = getConstructorName constructor
        [expr] = catMaybes exps
        pattern' = withDummyLocation $ PatternConstr constr idents
    return . withDummyLocation $ AltSimple pattern' expr
