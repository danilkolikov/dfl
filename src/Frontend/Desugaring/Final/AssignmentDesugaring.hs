{- |
Module      :  Frontend.Desugaring.Final.AssignmentDesugaring
Description :  Final desugaring of assignments
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of assignments and related nodes
-}
module Frontend.Desugaring.Final.AssignmentDesugaring
    ( desugarExpressions
    , desugarTopLevelAssignments
    , desugarAssignments
    , resolveRecords
    ) where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State as S (get, modify, runStateT)
import Data.Functor ((<$))
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)

import Frontend.Desugaring.Final.Ast hiding (getDataTypeConstructors)
import Frontend.Desugaring.Final.PatternDesugaring
import Frontend.Desugaring.Final.Processor
import qualified Frontend.Desugaring.Final.RecordDesugaring as RD
    ( desugarAssignment
    , runRecordDesugaringProcessor
    )
import qualified Frontend.Desugaring.Final.ResolvedAst as R
import Frontend.Desugaring.Final.Util
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Desugar list of top level declarations to expressions
desugarExpressions ::
       (WithLocation R.Exp -> DesugaringProcessor (WithLocation Exp))
    -> [WithLocation I.TopDecl]
    -> DesugaringProcessor Expressions
desugarExpressions desugarExp topDecls = do
    let assignments = collectAssignments topDecls
    resolvedAssignments <- resolveRecords assignments
    desugarTopLevelAssignments desugarExp resolvedAssignments

-- | Resolve records in assignments
resolveRecords :: [WithLocation I.Assignment] -> DesugaringProcessor [WithLocation R.Assignment]
resolveRecords assignments = do
  state <- S.get
  let resolvedAssignments =
          RD.runRecordDesugaringProcessor
              (mapM RD.desugarAssignment assignments)
              (getDataTypeFields state)
              (getDataTypeConstructors state)
  case resolvedAssignments of
    Left err -> raiseError $ DesugaringErrorRecord err
    Right res -> return res

-- | Desugars assignments and defines functions
desugarTopLevelAssignments ::
       (WithLocation R.Exp -> DesugaringProcessor (WithLocation Exp))
    -> [WithLocation R.Assignment]
    -> DesugaringProcessor Expressions
desugarTopLevelAssignments desugarExp assignments = do
    desugared <- desugarAssignments desugarExp assignments
    let defineExpression (_, expr) = defineFunctionName (getExpressionName expr)
    mapM_ defineExpression $ HM.toList desugared
    return desugared

collectAssignments :: [WithLocation I.TopDecl] -> [WithLocation I.Assignment]
collectAssignments [] = []
collectAssignments (t:rest) =
    let collected = collectAssignments rest
     in case getValue t of
            (I.TopDeclAssignment a) -> a : collected
            _ -> collected

-- | Desugar list of assignments to expressions
desugarAssignments ::
       (WithLocation R.Exp -> DesugaringProcessor (WithLocation Exp))
    -> [WithLocation R.Assignment]
    -> DesugaringProcessor Expressions
desugarAssignments desugarExp assignments = do
    (grouped, patterns) <- groupAssignments assignments
    let desugarGroup (groupName, AssignmentsGroup name exps type') = do
            nonEmpty <-
                case exps of
                    [] ->
                        raiseError $
                        DesugaringErrorMissingExpressionDefinition name
                    (e:rest) -> return (e NE.:| rest)
            let desugarRHS (pat, exp') = do
                    desugaredExp' <- desugarExp exp'
                    return (pat, desugaredExp')
            desugaredNonEmpty <- mapM desugarRHS nonEmpty
            desugaredExp <- mergeExpressions name desugaredNonEmpty
            return (groupName, Expression name desugaredExp type')
    desugaredGroups <- mapM desugarGroup (HM.toList grouped)
    let desugarSinglePattern (singlePattern, exp')
          -- Special case of a function without arguments
            | (R.PatternVar name Nothing) <- getValue singlePattern = do
                desugaredExp <- lift $ desugarExp exp'
                let expression = Expression name desugaredExp Nothing
                S.modify $ HM.insert (getValue name) expression
            | otherwise = do
                expIdent <- lift generateNewIdent'
                desugaredExp <- lift $ desugarExp exp'
                let expression = Expression expIdent desugaredExp Nothing
                S.modify $ HM.insert (getValue expIdent) expression
                prepared <- lift $ preparePatterns singlePattern
                let processPattern (_, Nothing) = return ()
                    processPattern (pat, Just var) = do
                        expScope <- S.get
                        case HM.lookup (getValue var) expScope of
                            Just found ->
                                lift $
                                raiseError $
                                DesugaringErrorIdentifierIsAlreadyDefined
                                    var
                                    (getExpressionName found)
                            Nothing -> return ()
                        let varExp = withDummyLocation $ ExpVar var
                            prepareCase ident elseIdent p =
                                desugarPattern ident p varExp elseIdent
                        case' <-
                            lift $
                            desugarAlts expIdent prepareCase (pat NE.:| [])
                        let resultExp = Expression var case' Nothing
                        S.modify $ HM.insert (getValue var) resultExp
                mapM_ processPattern prepared
        processAllPatterns = mapM_ desugarSinglePattern patterns
        scope = HM.fromList desugaredGroups
    (_, finalScope) <- S.runStateT processAllPatterns scope
    return finalScope

patternWithoutVariables :: WithLocation R.Pattern -> WithLocation R.Pattern
patternWithoutVariables pat =
    case getValue pat of
        R.PatternWildcard -> pat
        R.PatternConst _ -> pat
        R.PatternVar _ Nothing -> withDummyLocation R.PatternWildcard
        R.PatternVar _ (Just p) -> patternWithoutVariables p
        R.PatternConstr name args ->
            R.PatternConstr name (map patternWithoutVariables args) <$ pat

preparePatterns ::
       WithLocation R.Pattern
    -> DesugaringProcessor [(WithLocation R.Pattern, Maybe (WithLocation Ident))]
preparePatterns pat =
    case getValue pat of
        R.PatternWildcard -> return [(pat, Nothing)]
        R.PatternConst _ -> return [(pat, Nothing)]
        R.PatternVar name Nothing -> return [(pat, Just name)]
        R.PatternVar name (Just p) -> do
            preparedRest <- preparePatterns p
            return $
                ( R.PatternVar name (Just $ patternWithoutVariables p) <$ pat
                , Just name) :
                preparedRest
        R.PatternConstr name args -> do
            withVariables <- mapM preparePatterns args
            let withoutVariables = map patternWithoutVariables args
                prepared = zip withVariables withoutVariables
                makePatterns _ [] = []
                makePatterns prev (arg:rest) =
                    let madeRest = makePatterns (prev ++ [arg]) rest
                        prevWithoutVariables = map snd prev
                        argWithVariables = fst arg
                        restWithoutVariables = map snd rest
                        resPatterns =
                            [ (R.PatternConstr name newArgs <$ pat, var)
                            | (argPat, var) <- argWithVariables
                            , let newArgs =
                                      prevWithoutVariables ++
                                      [argPat] ++ restWithoutVariables
                            ]
                     in resPatterns ++ madeRest
                res = makePatterns [] prepared
                filtered = filter (isJust . snd) res
            return filtered

data AssignmentsGroup =
    AssignmentsGroup (WithLocation Ident)
                     [(NE.NonEmpty (WithLocation R.Pattern), WithLocation R.Exp)]
                     (Maybe TypeSignature)

groupAssignments ::
       [WithLocation R.Assignment]
    -> DesugaringProcessor ( HM.HashMap Ident AssignmentsGroup
                           , [(WithLocation R.Pattern, WithLocation R.Exp)])
groupAssignments [] = return (HM.empty, [])
groupAssignments (f:rest) = do
    (groupedRest, patterns) <- groupAssignments rest
    case getValue f of
        R.AssignmentName name pats exp' ->
            let name' = getValue name
                res = (pats, exp')
                assignment =
                    case HM.lookup name' groupedRest of
                        Nothing -> AssignmentsGroup name [res] Nothing
                        Just (AssignmentsGroup _ assignments type') ->
                            AssignmentsGroup name (res : assignments) type'
             in return (HM.insert name' assignment groupedRest, patterns)
        R.AssignmentPattern pattern' exp' ->
            let res = (pattern', exp')
             in return (groupedRest, res : patterns)
        R.AssignmentType name context type' -> do
            let name' = getValue name
                typeSignature =
                    TypeSignature (map desugarConstraint context) type'
            assignment <-
                case HM.lookup name' groupedRest of
                    Nothing ->
                        return $ AssignmentsGroup name [] (Just typeSignature)
                    Just (AssignmentsGroup _ assignments foundType) ->
                        case foundType of
                            Nothing ->
                                return $
                                AssignmentsGroup
                                    name
                                    assignments
                                    (Just typeSignature)
                            Just _ ->
                                raiseError $
                                DesugaringErrorDuplicatedTypeDeclaration name
            return (HM.insert name' assignment groupedRest, patterns)

mergeExpressions ::
       WithLocation Ident
    -> NE.NonEmpty (NE.NonEmpty (WithLocation R.Pattern), WithLocation Exp)
    -> DesugaringProcessor (WithLocation Exp)
mergeExpressions name nonEmpty@((patterns, _) NE.:| rest) = do
    let nPatterns = NE.length patterns
        equalNumberOfArgs = all (\(pats, _) -> NE.length pats == nPatterns) rest
    unless equalNumberOfArgs $
        raiseError $ DesugaringErrorDifferentNumberOfArguments name
    desugarPatternsToAbstraction nPatterns nonEmpty
