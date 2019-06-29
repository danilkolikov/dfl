{- |
Module      :  Frontend.Desugaring.Final.ExpressionDesugaringAssignment
Description :  Desugaring of assignments
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of assignments
-}
module Frontend.Desugaring.Final.ExpressionDesugaringAssignment where

import Control.Monad (unless)
import Control.Monad.Trans.Except (throwE)
import Data.Either (partitionEithers)
import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, isJust)

import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.ExpressionDesugaringBase
import Frontend.Desugaring.Final.ExpressionDesugaringCase
    ( desugarCase
    , desugarPatternsToAbstraction
    )
import qualified Frontend.Desugaring.Final.ResolvedAst as R
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Desugar a list of prepared assignments into Expressions
desugarPreparedAssignments ::
       [WithLocation PreparedAssignment]
    -> ExpressionDesugaringProcessor Expressions
desugarPreparedAssignments assignments = do
    (methods, expressions) <- desugarPreparedAssignmentsWithMethods assignments
    let methodsWithoutImplementation =
            filter
                (\Method {getMethodDefault = def} -> def == Nothing)
                (map snd $ HM.toList methods)
    unless (null methodsWithoutImplementation) $
        raiseError $
        ExpressionDesugaringErrorMissingExpressionDefinition
            (getMethodName . head $ methodsWithoutImplementation)
    return expressions

-- | Desugar a list of prepared assignments into Methods
desugarPreparedMethods ::
  [WithLocation PreparedAssignment]
  -> ExpressionDesugaringProcessor Methods
desugarPreparedMethods assignments = do
    (methods, expressions) <- desugarPreparedAssignmentsWithMethods assignments
    let expressionsWithoutSignature =
            filter
                (\Expression {getExpressionType = sig} -> sig == Nothing)
                (map snd $ HM.toList expressions)
    unless (null expressionsWithoutSignature) $
        raiseError $
        ExpressionDesugaringErrorMissingMethodType
            (getExpressionName . head $ expressionsWithoutSignature)
    return methods

-- | Desugar a list of prepared assignments into Methods and Expressions
desugarPreparedAssignmentsWithMethods ::
       [WithLocation PreparedAssignment]
    -> ExpressionDesugaringProcessor (Methods, Expressions)
desugarPreparedAssignmentsWithMethods assignments = do
    (grouped, patterns) <- groupAssignments assignments
    desugaredGroups <- mapM desugarGroup (HM.toList grouped)
    patternExprs <- desugarAllPatterns patterns
    let (methods, expressions) = partitionEithers (concat desugaredGroups)
        expressionsMap = HM.fromList expressions
        methodsMap = HM.fromList methods
    finalExpressions <- addExpressions expressionsMap patternExprs
    return (methodsMap, finalExpressions)

-- | Group of assignments
data AssignmentsGroup =
    AssignmentsGroup (WithLocation Ident)
                     [(NE.NonEmpty (WithLocation R.Pattern), WithLocation Exp)]
                     (Maybe TypeSignature)

-- | Group assignments from the provided list of PreparedAssignment-s
groupAssignments ::
       [WithLocation PreparedAssignment]
    -> ExpressionDesugaringProcessor ( HM.HashMap Ident AssignmentsGroup
                                     , [( WithLocation R.Pattern
                                        , WithLocation Exp)])
groupAssignments [] = return (HM.empty, [])
groupAssignments (f:rest) = do
    (groupedRest, patterns) <- groupAssignments rest
    case getValue f of
        PreparedAssignmentName name pats exp' ->
            let name' = getValue name
                res = (pats, exp')
                assignment =
                    case HM.lookup name' groupedRest of
                        Nothing -> AssignmentsGroup name [res] Nothing
                        Just (AssignmentsGroup _ assignments type') ->
                            AssignmentsGroup name (res : assignments) type'
             in return (HM.insert name' assignment groupedRest, patterns)
        PreparedAssignmentPattern pattern' exp' ->
            let res = (pattern', exp')
             in return (groupedRest, res : patterns)
        PreparedAssignmentType name context type' -> do
            let name' = getValue name
                typeSignature = TypeSignature context type'
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
                                throwE $
                                ExpressionDesugaringErrorDuplicatedTypeDeclaration
                                    name
            return (HM.insert name' assignment groupedRest, patterns)

-- | Desugar single group of assignments
desugarGroup ::
       (Ident, AssignmentsGroup)
    -> ExpressionDesugaringProcessor [Either (Ident, Method) (Ident, Expression)]
desugarGroup (groupName, AssignmentsGroup name exps type') =
    case (exps, type') of
        ([], Nothing) -> error "Such combination is impossible"
        ([], Just signature) ->
            return $ [Left (groupName, Method name signature Nothing)]
        (e:rest, _) -> do
            mergedExp <- mergeExpressions name (e NE.:| rest)
            -- It can be either expression, or method with default implementation
            let expression = Right (groupName, Expression name mergedExp type')
            return $
                case type' of
                    Nothing -> [expression]
                    Just signature ->
                        let method =
                                Left
                                    ( groupName
                                    , Method name signature (Just mergedExp))
                         in [method, method]

-- | Merge multilple expressions into one
mergeExpressions ::
       WithLocation Ident
    -> NE.NonEmpty (NE.NonEmpty (WithLocation R.Pattern), WithLocation Exp)
    -> ExpressionDesugaringProcessor (WithLocation Exp)
mergeExpressions name nonEmpty@((patterns, exp') NE.:| rest) = do
    let nPatterns = NE.length patterns
        equalNumberOfArgs = all (\(pats, _) -> NE.length pats == nPatterns) rest
    unless equalNumberOfArgs $
        raiseError $ ExpressionDesugaringErrorDifferentNumberOfArguments name
    merged <- desugarPatternsToAbstraction nPatterns nonEmpty
    return $ exp' $> merged

-- | Add expressions to a map of expressions and check for duplications
addExpressions ::
       Expressions
    -> [(Ident, Expression)]
    -> ExpressionDesugaringProcessor Expressions
addExpressions exprs [] = return exprs
addExpressions exprs ((name, expr):rest) = do
    mergedRest <- addExpressions exprs rest
    case HM.lookup name mergedRest of
        Just found ->
            raiseError $
            ExpressionDesugaringErrorIdentifierIsAlreadyDefined
                (getExpressionName expr)
                (getExpressionName found)
        Nothing -> return $ HM.insert name expr mergedRest

-- | Desugar all pattern assignments
desugarAllPatterns ::
       [(WithLocation R.Pattern, WithLocation Exp)]
    -> ExpressionDesugaringProcessor [(Ident, Expression)]
desugarAllPatterns patterns = concat <$> mapM desugarSinglePattern patterns

-- | Desugar a single pattern assignment
desugarSinglePattern ::
       (WithLocation R.Pattern, WithLocation Exp)
    -> ExpressionDesugaringProcessor [(Ident, Expression)]
desugarSinglePattern (singlePattern, exp')
      -- Special case of a function without arguments
    | (R.PatternVar name Nothing) <- getValue singlePattern =
        let expression = Expression name exp' Nothing
         in return [(getValue name, expression)]
    | otherwise = do
        expIdent <- generateNewIdent'
        let expression = Expression expIdent exp' Nothing
            expPair = (getValue expIdent, expression)
            expVar = expIdent $> ExpVar expIdent
            patterns = getVariablesFromPattern singlePattern
        prepared <- mapM (processPattern expVar) patterns
        return $ expPair : catMaybes prepared

-- | Create an binding for a single variable from a pattern
processPattern ::
       WithLocation Exp
    -> (WithLocation R.Pattern, Maybe (WithLocation Ident))
    -> ExpressionDesugaringProcessor (Maybe (Ident, Expression))
processPattern _ (_, Nothing) = return Nothing
processPattern expVar (pat, Just var) = do
    let varExp = withDummyLocation $ ExpVar var
        alt = withDummyLocation $ PreparedAltSimple pat varExp
    case' <- desugarCase expVar (alt NE.:| [])
    let resultExp = Expression var (withDummyLocation case') Nothing
    return $ Just (getValue var, resultExp)

-- | Remove variables from a pattern
patternWithoutVariables :: WithLocation R.Pattern -> WithLocation R.Pattern
patternWithoutVariables pat =
    case getValue pat of
        R.PatternWildcard -> pat
        R.PatternConst _ -> pat
        R.PatternVar _ Nothing -> pat $> R.PatternWildcard
        R.PatternVar _ (Just p) -> patternWithoutVariables p
        R.PatternConstr name args ->
            pat $> R.PatternConstr name (map patternWithoutVariables args)

-- | Find all variables in a pattern, and for each of them create a pattern,
-- | where other variables are removed
getVariablesFromPattern ::
       WithLocation R.Pattern
    -> [(WithLocation R.Pattern, Maybe (WithLocation Ident))]
getVariablesFromPattern pat =
    case getValue pat of
        R.PatternWildcard -> [(pat, Nothing)]
        R.PatternConst _ -> [(pat, Nothing)]
        R.PatternVar name Nothing -> [(pat, Just name)]
        R.PatternVar name (Just p) ->
            let variables = getVariablesFromPattern p
             in ( R.PatternVar name (Just $ patternWithoutVariables p) <$ pat
                , Just name) :
                variables
        R.PatternConstr name args ->
            let variables = fmap getVariablesFromPattern args
                withoutVariables = map patternWithoutVariables args
                prepared = zip variables withoutVariables
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
             in filter (isJust . snd) res
