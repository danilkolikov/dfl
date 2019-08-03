{- |
Module      :  Frontend.Desugaring.Final.ExpressionDesugaringAssignment
Description :  Desugaring of assignments
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of assignments
-}
module Frontend.Desugaring.Final.ExpressionDesugaringAssignment where

import Control.Monad (foldM, unless)
import Control.Monad.Trans.Except (throwE)
import Data.Bifunctor (first)
import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)

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
                (\Method {getMethodDefault = def} -> isNothing def)
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
                (\Expression {getExpressionType = sig} -> isNothing sig)
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
    (grouped, patterns, types) <- groupAssignments assignments
    desugaredGroups <- mapM desugarGroup (HM.elems grouped)
    patternExprs <- concat <$> mapM desugarSinglePattern patterns
    ensureNoIntersection desugaredGroups patternExprs
    let (methodsMap, expressionsMap) =
            splitToMethodsAndExpressions types $ desugaredGroups ++ patternExprs
    return (methodsMap, expressionsMap)

-- | Group of assignments
data AssignmentsGroup =
    AssignmentsGroup (WithLocation Ident)
                     (NE.NonEmpty ( NE.NonEmpty (WithLocation R.Pattern)
                                  , WithLocation Exp))

-- | Output of the function "groupAssignments"
type GroupedAssignments
     = ( HM.HashMap Ident AssignmentsGroup
       , [(WithLocation R.Pattern, WithLocation Exp)]
       , [(WithLocation Ident, TypeSignature)])

-- | Group assignments from the provided list of PreparedAssignment-s
groupAssignments ::
       [WithLocation PreparedAssignment]
    -> ExpressionDesugaringProcessor GroupedAssignments
groupAssignments = foldM processSingle (HM.empty, [], [])
  where
    processSingle ::
           GroupedAssignments
        -> WithLocation PreparedAssignment
        -> ExpressionDesugaringProcessor GroupedAssignments
    processSingle (assignments, patterns, types) f =
        case getValue f of
            PreparedAssignmentName name pats exp' ->
                let name' = getValue name
                    res = (pats, exp')
                    assignment =
                        case HM.lookup name' assignments of
                            Nothing -> AssignmentsGroup name (res NE.:| [])
                            Just (AssignmentsGroup _ group) ->
                                AssignmentsGroup name (res NE.<| group)
                 in return
                        ( HM.insert name' assignment assignments
                        , patterns
                        , types)
            PreparedAssignmentPattern pattern' exp' ->
                let res = (pattern', exp')
                 in return (assignments, res : patterns, types)
            PreparedAssignmentType name context type' -> do
                let name' = getValue name
                    typeSignature = TypeSignature context type'
                unless (all (\(n, _) -> getValue n /= name') types) $
                    throwE $
                    ExpressionDesugaringErrorDuplicatedTypeDeclaration name
                return (assignments, patterns, (name, typeSignature) : types)

-- | Splits the set of signatures and expressions to methods and expressions
splitToMethodsAndExpressions ::
       [(WithLocation Ident, TypeSignature)]
    -> [(WithLocation Ident, WithLocation Exp)]
    -> (HM.HashMap Ident Method, HM.HashMap Ident Expression)
splitToMethodsAndExpressions types exprs =
    let typesMap = HM.fromList $ map (first getValue) types
        processExp (name, exp') =
            let name' = getValue name
             in case HM.lookup name' typesMap of
                    Nothing ->
                        ( HM.empty
                        , HM.singleton name' $ Expression name exp' Nothing)
                    Just type' ->
                        ( HM.singleton name' $ Method name type' (Just exp')
                        , HM.singleton name' $ Expression name exp' (Just type'))
        processType (name, type') =
            (HM.singleton (getValue name) $ Method name type' Nothing, HM.empty)
        expNames = HS.fromList $ map (getValue . fst) exprs
        typeNames = HM.keysSet typesMap `HS.difference` expNames
        methods =
            filter (\(name, _) -> getValue name `HS.member` typeNames) types
     in mconcat $ map processExp exprs ++ map processType methods

-- | Desugar single group of assignments
desugarGroup ::
       AssignmentsGroup
    -> ExpressionDesugaringProcessor (WithLocation Ident, WithLocation Exp)
desugarGroup (AssignmentsGroup name exps) = do
    mergedExp <- mergeExpressions name exps
    return (name, mergedExp)

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

-- | Ensure that there is no intersection between two group of expressions
ensureNoIntersection ::
       [(WithLocation Ident, WithLocation Exp)]
    -> [(WithLocation Ident, WithLocation Exp)]
    -> ExpressionDesugaringProcessor ()
ensureNoIntersection a b =
    let makeMap = HM.fromList . map (\(n, _) -> (getValue n, n))
        aNames = makeMap a
        bNames = makeMap b
        sameNames =
            HS.toList $ HM.keysSet aNames `HS.intersection` HM.keysSet bNames
        findName names = fromJust $ HM.lookup (head sameNames) names
     in unless (null sameNames) $
        raiseError $
        ExpressionDesugaringErrorIdentifierIsAlreadyDefined
            (findName aNames)
            (findName bNames)

-- | Desugar a single pattern assignment
desugarSinglePattern ::
       (WithLocation R.Pattern, WithLocation Exp)
    -> ExpressionDesugaringProcessor [(WithLocation Ident, WithLocation Exp)]
desugarSinglePattern (singlePattern, exp')
      -- Special case of a function without arguments
    | (R.PatternVar name Nothing) <- getValue singlePattern =
        return [(name, exp')]
    | otherwise = do
        expIdent <- generateNewIdent'
        let expPair = (expIdent, exp')
            expVar = expIdent $> ExpVar expIdent
            patterns = getVariablesFromPattern singlePattern
        prepared <- mapM (processPattern expVar) patterns
        return $ expPair : catMaybes prepared

-- | Create an binding for a single variable from a pattern
processPattern ::
       WithLocation Exp
    -> (WithLocation R.Pattern, Maybe (WithLocation Ident))
    -> ExpressionDesugaringProcessor (Maybe ( WithLocation Ident
                                            , WithLocation Exp))
processPattern _ (_, Nothing) = return Nothing
processPattern expVar (pat, Just var) = do
    let varExp = withDummyLocation $ ExpVar var
        alt = withDummyLocation $ PreparedAltSimple pat varExp
    case' <- desugarCase expVar (alt NE.:| [])
    let resultExp = withDummyLocation case'
    return $ Just (var, resultExp)

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
