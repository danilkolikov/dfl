{- |
Module      :  Frontend.Desugaring.Final.ClassDesugaring
Description :  Final desugaring of classes
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of classes
-}
module Frontend.Desugaring.Final.ClassDesugaring where

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE (NonEmpty(..))

import Frontend.Desugaring.Final.AssignmentDesugaring
    ( desugarTopLevelAssignments
    )
import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.ExpressionDesugaring (desugarExp)
import Frontend.Desugaring.Final.Processor
import Frontend.Desugaring.Final.Util
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (WithLocation(..))

-- | Find classes among the list of top declarations and desugar them
desugarClasses :: [WithLocation I.TopDecl] -> DesugaringProcessor Classes
desugarClasses = collectHashMap desugarClass

-- | Desugar a single top declaration to a class, or return Nothing
desugarClass :: I.TopDecl -> DesugaringProcessor (Maybe (Ident, Class))
desugarClass (I.TopDeclClass context name param methods) = do
    let desugaredContext = map desugarSimpleClass context
        makeAssignment classAssignment =
            classAssignment $>
            case getValue classAssignment of
                I.ClassAssignmentName name' pats exp' ->
                    case pats of
                        [] ->
                            let pat = name' $> I.PatternVar name Nothing
                             in I.AssignmentPattern pat exp'
                        (f:rest) -> I.AssignmentName name (f NE.:| rest) exp'
                I.ClassAssignmentType name' context' type' ->
                    I.AssignmentType name' context' type'
        assignments = map makeAssignment methods
    -- We need to define top-level functions
    desugaredMethods <- desugarTopLevelAssignments desugarExp assignments
    return . Just $
        (getValue name, Class desugaredContext name param desugaredMethods)
desugarClass _ = return Nothing
