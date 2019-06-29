{- |
Module      :  Frontend.Desugaring.Final.AssignmentDesugaring
Description :  Final desugaring of assignments
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of assignments and related nodes
-}
module Frontend.Desugaring.Final.AssignmentDesugaring
    ( desugarTopLevelAssignments
    , desugarClassAssignments
    , desugarInstanceAssignments
    ) where

import qualified Control.Monad.Trans.State as S (get)
import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Final.Ast hiding (getDataTypeConstructors)
import qualified Frontend.Desugaring.Final.ExpressionDesugaring as ED
import Frontend.Desugaring.Final.ExpressionDesugaringBase
    ( ExpressionDesugaringProcessor
    , runExpressionDesugaringProcessor
    )
import Frontend.Desugaring.Final.Processor
import qualified Frontend.Desugaring.Final.RecordDesugaring as RD
    ( desugarAssignment
    , runRecordDesugaringProcessor
    )
import qualified Frontend.Desugaring.Final.ResolvedAst as R
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (WithLocation(..))

-- | Desugar a list of top level declarations to expressions
desugarTopLevelAssignments ::
       [WithLocation I.TopDecl] -> DesugaringProcessor Expressions
desugarTopLevelAssignments topDecls = do
    let assignments = collectAssignments topDecls
    resolvedAssignments <- resolveRecords assignments
    desugared <-
        wrapExpressionDesugaring $ ED.desugarAssignments resolvedAssignments
    let functions = map (getExpressionName . snd) $ HM.toList desugared
    mapM_ defineFunctionName functions
    return desugared

-- | Desugar a list of class assignments to expressions and methods
desugarClassAssignments ::
       [WithLocation I.ClassAssignment]
    -> DesugaringProcessor Methods
desugarClassAssignments classAssignments = do
    let makeAssignment classAssignment =
            classAssignment $>
            case getValue classAssignment of
                I.ClassAssignmentName name' pats exp' ->
                    case pats of
                        [] ->
                            let pat = name' $> I.PatternVar name' Nothing
                             in I.AssignmentPattern pat exp'
                        (f:rest) -> I.AssignmentName name' (f NE.:| rest) exp'
                I.ClassAssignmentType name' context' type' ->
                    I.AssignmentType name' context' type'
        assignments = map makeAssignment classAssignments
    resolvedAssignments <- resolveRecords assignments
    -- We need to collect method definitions as well
    desugared <-
        wrapExpressionDesugaring $
        ED.desugarMethods resolvedAssignments
    -- All methods and functions declared in a class are top level
    let methods = map (getMethodName . snd) $ HM.toList desugared
    mapM_ defineFunctionName methods
    return desugared

-- | Desugar a list of instance assignments to expressions
desugarInstanceAssignments ::
       [WithLocation I.InstAssignment] -> DesugaringProcessor Expressions
desugarInstanceAssignments instAssignments = do
    let makeAssignment instAssignment =
            instAssignment $>
            case getValue instAssignment of
                I.InstAssignmentName name' pats exp' ->
                    case pats of
                        [] ->
                            let pat = name' $> I.PatternVar name' Nothing
                             in I.AssignmentPattern pat exp'
                        (f:rest) -> I.AssignmentName name' (f NE.:| rest) exp'
        assignments = map makeAssignment instAssignments
    resolvedAssignments <- resolveRecords assignments
    -- We don't need to declare there expressions on the top level
    wrapExpressionDesugaring $ ED.desugarAssignments resolvedAssignments

-- | Resolve records in assignments
resolveRecords ::
       [WithLocation I.Assignment]
    -> DesugaringProcessor [WithLocation R.Assignment]
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

-- | Wrap processor of expression desugaring into DesugaringProcessor
wrapExpressionDesugaring ::
       ExpressionDesugaringProcessor a -> DesugaringProcessor a
wrapExpressionDesugaring edp =
    let (desugaringResult, _) = runExpressionDesugaringProcessor edp 0
     in case desugaringResult of
            Left err -> raiseError $ DesugaringErrorExpression err
            Right res -> return res

-- | Find assignments in the list of top declarations
collectAssignments :: [WithLocation I.TopDecl] -> [WithLocation I.Assignment]
collectAssignments [] = []
collectAssignments (t:rest) =
    let collected = collectAssignments rest
     in case getValue t of
            (I.TopDeclAssignment a) -> a : collected
            _ -> collected
