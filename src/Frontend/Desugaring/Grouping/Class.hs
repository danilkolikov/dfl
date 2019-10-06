{- |
Module      :  Frontend.Desugaring.Grouping.Class
Description :  Grouping of classes
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Grouping of classes
-}
module Frontend.Desugaring.Grouping.Class where

import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Grouping.Assignment
import Frontend.Desugaring.Grouping.Ast
import Frontend.Desugaring.Grouping.Base
import Frontend.Desugaring.Grouping.Util
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (WithLocation(..))

-- | Find classes among the list of top declarations and desugar them
groupClasses :: [WithLocation I.TopDecl] -> GroupingProcessor (Classes Exp)
groupClasses = collectHashMap processClass

-- | Desugar a single top declaration to a class, or return Nothing
processClass :: I.TopDecl -> GroupingProcessor (Maybe (Ident, Class Exp))
processClass (I.TopDeclClass context name param methods) = do
    let wrappedName = wrapIdent name
        wrappedContext = map wrapSimpleClass context
    groupedMethods <- groupClassAssignments methods
    let resultClass =
            Class
                { getClassContext = wrappedContext
                , getClassName = wrappedName
                , getClassParam = wrapIdent param
                , getClassMethods = groupedMethods
                }
    -- Define class and methods
    defineTypeName wrappedName
    mapM_ (defineExpressionName . (getMethodName . snd)) $
        HM.toList groupedMethods
    return . Just $ (getValue wrappedName, resultClass)
processClass _ = return Nothing

-- | Desugar a list of class assignments to expressions and methods
groupClassAssignments ::
       [WithLocation I.ClassAssignment] -> GroupingProcessor (Methods Exp)
groupClassAssignments classAssignments =
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
                I.ClassAssignmentFixity name' fixity prec ->
                    I.AssignmentFixity name' fixity prec
        assignments = map makeAssignment classAssignments
     in groupMethods assignments
