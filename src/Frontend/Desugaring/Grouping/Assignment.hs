{- |
Module      :  Frontend.Desugaring.Grouping.Assignment
Description :  Grouping of assignments
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Grouping of assignments and related nodes
-}
module Frontend.Desugaring.Grouping.Assignment where

import Control.Monad (foldM, unless)
import Data.Bifunctor (first)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, isNothing, mapMaybe)

import Core.PredefinedIdents
import Frontend.Desugaring.Grouping.Ast
import Frontend.Desugaring.Grouping.Base
import Frontend.Desugaring.Grouping.Expression
import Frontend.Desugaring.Grouping.PatternAssignment
import Frontend.Desugaring.Grouping.Util
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Desugar a list of top level declarations to expressions
groupTopLevelAssignments ::
       [WithLocation I.TopDecl] -> GroupingProcessor (Expressions Exp)
groupTopLevelAssignments topDecls = do
    let selectAssignment t =
            case getValue t of
                I.TopDeclAssignment a -> Just a
                _ -> Nothing
        assignments = mapMaybe selectAssignment topDecls
    grouped <- groupAssignments assignments
    let functions = map (getExpressionName . snd) $ HM.toList grouped
    mapM_ defineExpressionName functions
    return grouped

-- | Desugar a list of prepared assignments into Expressions
groupAssignments ::
       [WithLocation I.Assignment] -> GroupingProcessor (Expressions Exp)
groupAssignments assignments = do
    (methods, expressions) <- groupAssignmentsWithMethods assignments
    let methodsWithoutImplementation =
            filter
                (\Method {getMethodBody = def} -> isNothing def)
                (map snd $ HM.toList methods)
    unless (null methodsWithoutImplementation) $
        raiseGroupingError $
        GroupingProcessorErrorMissingExpressionDefinition
            (getMethodName . head $ methodsWithoutImplementation)
    return expressions

-- | Desugar a list of prepared assignments into Methods
groupMethods :: [WithLocation I.Assignment] -> GroupingProcessor (Methods Exp)
groupMethods assignments = do
    (methods, expressions) <- groupAssignmentsWithMethods assignments
    let expressionsWithoutSignature =
            filter
                (\Expression {getExpressionType = sig} -> isNothing sig)
                (map snd $ HM.toList expressions)
    unless (null expressionsWithoutSignature) $
        raiseGroupingError $
        GroupingProcessorErrorMissingMethodType
            (getExpressionName . head $ expressionsWithoutSignature)
    return methods

-- | Desugar a list of prepared assignments into Methods and Expressions
groupAssignmentsWithMethods ::
       [WithLocation I.Assignment]
    -> GroupingProcessor (Methods Exp, Expressions Exp)
groupAssignmentsWithMethods assignments = do
    GroupedAssignments { getGroupedAssignmentsGroups = grouped
                       , getGroupedAssignmentsPatterns = patterns
                       , getGroupedAssignmentsTypes = types
                       , getGroupedAssignmentsFixities = fixities
                       } <- groupAssignments' assignments
    groups <- mapM processGroup (HM.elems grouped)
    let patternExprs = concatMap groupSinglePattern patterns
    ensureNoIntersection groups patternExprs
    let (methodsMap, expressionsMap) =
            splitToMethodsAndExpressions types fixities $ groups ++ patternExprs
    return (methodsMap, expressionsMap)

-- | Group assignments from the provided list of Assignment-s
groupAssignments' ::
       [WithLocation I.Assignment] -> GroupingProcessor GroupedAssignments
groupAssignments' = foldM processSingle mempty
  where
    processSingle ::
           GroupedAssignments
        -> WithLocation I.Assignment
        -> GroupingProcessor GroupedAssignments
    processSingle gr@GroupedAssignments { getGroupedAssignmentsGroups = assignments
                                        , getGroupedAssignmentsPatterns = patterns
                                        , getGroupedAssignmentsTypes = types
                                        , getGroupedAssignmentsFixities = fixities
                                        } f =
        case getValue f of
            I.AssignmentName name pats exp' -> do
                let wrappedName = wrapIdent name
                    name' = getValue wrappedName
                    wrappedPats = fmap wrapPattern pats
                wrappedExp <- wrapExpression groupAssignments exp'
                let res = (wrappedPats, wrappedExp)
                    makeGroup = AssignmentsGroup wrappedName
                    assignment =
                        case HM.lookup name' assignments of
                            Nothing -> makeGroup (res NE.:| [])
                            Just (AssignmentsGroup _ group) ->
                                makeGroup (res NE.<| group)
                return $
                    gr <>
                    mempty
                        { getGroupedAssignmentsGroups =
                              HM.insert name' assignment assignments
                        }
            I.AssignmentPattern pattern' exp' -> do
                wrappedExp <- wrapExpression groupAssignments exp'
                let res = (wrapPattern pattern', wrappedExp)
                return $
                    gr <>
                    mempty {getGroupedAssignmentsPatterns = res : patterns}
            I.AssignmentType name context type' -> do
                let wrappedName = wrapIdent name
                    name' = getValue wrappedName
                    wrappedContext = map wrapConstraint context
                    typeSignature =
                        TypeSignature wrappedContext (wrapType type')
                unless (all (\(n, _) -> getValue n /= name') types) $
                    raiseGroupingError $
                    GroupingProcessorErrorDuplicatedTypeDeclaration wrappedName
                return $
                    gr <>
                    mempty
                        { getGroupedAssignmentsTypes =
                              (wrappedName, typeSignature) : types
                        }
            I.AssignmentFixity name fixity prec -> do
                let wrappedName = wrapIdent name
                    name' = getValue wrappedName
                    fixitySignature = FixitySignature fixity prec
                unless (all (\(n, _) -> getValue n /= name') fixities) $
                    raiseGroupingError $
                    GroupingProcessorErrorDuplicatedFixityDeclaration
                        wrappedName
                return $
                    gr <>
                    mempty
                        { getGroupedAssignmentsFixities =
                              (wrappedName, fixitySignature) : fixities
                        }

-- | Splits the set of signatures and expressions to methods and expressions
splitToMethodsAndExpressions ::
       [(WithLocation Ident, TypeSignature)]
    -> [(WithLocation Ident, FixitySignature)]
    -> [(WithLocation Ident, WithLocation Exp)]
    -> (Methods Exp, Expressions Exp)
splitToMethodsAndExpressions types fixities exprs =
    let typesMap = HM.fromList $ map (first getValue) types
        fixitiesMap = HM.fromList $ map (first getValue) fixities
        processExp (name, exp') =
            let name' = getValue name
                fixity = HM.lookup name' fixitiesMap
             in case HM.lookup name' typesMap of
                    Nothing ->
                        ( HM.empty
                        , HM.singleton name' $
                          Expression name exp' Nothing fixity)
                    Just type' ->
                        ( HM.singleton name' $
                          Method name type' (Just exp') Nothing
                        , HM.singleton name' $
                          Expression name exp' (Just type') fixity)
        processType (name, type') =
            ( HM.singleton (getValue name) $ Method name type' Nothing Nothing
            , HM.empty)
        expNames = HS.fromList $ map (getValue . fst) exprs
        typeNames = HM.keysSet typesMap `HS.difference` expNames
        methods =
            filter (\(name, _) -> getValue name `HS.member` typeNames) types
     in mconcat $ map processExp exprs ++ map processType methods

-- | Desugars a single group of assignments
processGroup ::
       AssignmentsGroup
    -> GroupingProcessor (WithLocation Ident, WithLocation Exp)
processGroup (AssignmentsGroup name nonEmpty)
    | ((firstPatterns, _) NE.:| rest) <- nonEmpty = do
        let nPatterns = NE.length firstPatterns
            equalNumberOfArgs =
                all (\(pats, _) -> NE.length pats == nPatterns) rest
        unless equalNumberOfArgs $
            raiseGroupingError $
            GroupingProcessorErrorDifferentNumberOfArguments name
        let makeIdent =
                withDummyLocation .
                IdentGenerated .
                GeneratedIdent GeneratedIdentEnvironmentGrouping
            argIdents = NE.fromList $ map makeIdent [0 .. nPatterns - 1]
            argExps = fmap (withDummyLocation . ExpVar) argIdents
            argPatterns =
                fmap (withDummyLocation . (`PatternVar` Nothing)) argIdents
            tupleName = withDummyLocation . IdentUserDefined $ tUPLE nPatterns
            tupleExp = withDummyLocation $ ExpConstr tupleName
            tuplePattern args =
                if nPatterns == 1
                    then head args
                    else withDummyLocation $ PatternConstr tupleName args
            argExp =
                if nPatterns == 1
                    then NE.head argExps
                    else withDummyLocation $ ExpApplication tupleExp argExps
            makeAlt (patterns, expr) =
                withDummyLocation $
                AltSimple (tuplePattern $ NE.toList patterns) expr
            case' = withDummyLocation $ ExpCase argExp (fmap makeAlt nonEmpty)
            merged = withDummyLocation $ ExpAbstraction argPatterns case'
        return (name, merged)

-- | Ensure that there is no intersection between two group of expressions
ensureNoIntersection ::
       [(WithLocation Ident, WithLocation Exp)]
    -> [(WithLocation Ident, WithLocation Exp)]
    -> GroupingProcessor ()
ensureNoIntersection a b =
    let makeMap = HM.fromList . map (\(n, _) -> (getValue n, n))
        aNames = makeMap a
        bNames = makeMap b
        sameNames =
            HS.toList $ HM.keysSet aNames `HS.intersection` HM.keysSet bNames
        findName names = fromJust $ HM.lookup (head sameNames) names
     in unless (null sameNames) $
        raiseGroupingError $
        GroupingProcessorErrorNameConflict (findName aNames) (findName bNames)
