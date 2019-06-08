{- |
Module      :  Frontend.Desugaring.Final.InstanceDesugaring
Description :  Final desugaring of classes
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of classes
-}
module Frontend.Desugaring.Final.InstanceDesugaring where

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE (NonEmpty(..))

import Frontend.Desugaring.Final.AssignmentDesugaring (desugarAssignments)
import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.ExpressionDesugaring (desugarExp)
import Frontend.Desugaring.Final.Processor
import Frontend.Desugaring.Final.Util
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (WithLocation(..))

-- | Find instances among the list of top declarations and desugar them
desugarInstances :: [WithLocation I.TopDecl] -> DesugaringProcessor Instances
desugarInstances = collectHashMap desugarInstance

-- | Desugar a single top declaration to a class, or return Nothing
desugarInstance :: I.TopDecl -> DesugaringProcessor (Maybe (Ident, Instance))
desugarInstance (I.TopDeclInstance context className inst methods) = do
    let desugaredContext = map desugarSimpleClass context
        I.Inst name params = getValue inst
        makeAssignment classAssignment =
            classAssignment $>
            case getValue classAssignment of
                I.InstAssignmentName name' pats exp' ->
                    case pats of
                        [] ->
                            let pat = name' $> I.PatternVar name Nothing
                             in I.AssignmentPattern pat exp'
                        (f:rest) -> I.AssignmentName name (f NE.:| rest) exp'
        assignments = map makeAssignment methods
    -- We don't need to define top-level functions here
    desugaredMethods <- desugarAssignments desugarExp assignments
    return . Just $
        ( getValue name
        , Instance desugaredContext className name params desugaredMethods)
desugarInstance _ = return Nothing
