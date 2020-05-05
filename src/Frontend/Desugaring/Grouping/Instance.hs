{- |
Module      :  Frontend.Desugaring.Grouping.Instance
Description :  Grouping of instances
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Grouping of instances
-}
module Frontend.Desugaring.Grouping.Instance where

import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Grouping.Assignment
import Frontend.Desugaring.Grouping.Ast
import Frontend.Desugaring.Grouping.Base
import Frontend.Desugaring.Grouping.Util
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (WithLocation(..))

-- | Find instances among the list of top declarations and group them
groupInstances :: [WithLocation I.TopDecl] -> GroupingProcessor (Instances Exp)
groupInstances = collectList groupInstance

-- | Desugar a single top declaration to a class, or return Nothing
groupInstance :: I.TopDecl -> GroupingProcessor (Maybe (Instance Exp))
groupInstance (I.TopDeclInstance context className inst methods) = do
    let wrappedContext = map wrapSimpleClass context
        I.Inst name params = getValue inst
        wrappedName = wrapIdent name
        wrappedParams = map wrapIdent params
        wrappedClassName = wrapIdent className
    groupedMethods <- groupInstanceAssignments methods
    return . Just $
        Instance
            wrappedContext
            wrappedClassName
            wrappedName
            wrappedParams
            groupedMethods
groupInstance _ = return Nothing

-- | Desugar a list of instance assignments to expressions
groupInstanceAssignments ::
       [WithLocation I.InstAssignment]
    -> GroupingProcessor (HM.HashMap Ident (WithLocation Exp))
groupInstanceAssignments instAssignments = do
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
    grouped <- groupAssignments assignments
    return $ HM.map getExpressionBody grouped
