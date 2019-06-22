{- |
Module      :  Frontend.Desugaring.Final.InstanceDesugaring
Description :  Final desugaring of classes
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of classes
-}
module Frontend.Desugaring.Final.InstanceDesugaring where

import Frontend.Desugaring.Final.AssignmentDesugaring
    ( desugarInstanceAssignments
    )
import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.Processor
import Frontend.Desugaring.Final.Utils
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
    desugaredMethods <- desugarInstanceAssignments methods
    return . Just $
        ( getValue name
        , Instance desugaredContext className name params desugaredMethods)
desugarInstance _ = return Nothing
