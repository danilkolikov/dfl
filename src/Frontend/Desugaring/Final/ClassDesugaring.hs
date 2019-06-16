{- |
Module      :  Frontend.Desugaring.Final.ClassDesugaring
Description :  Final desugaring of classes
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of classes
-}
module Frontend.Desugaring.Final.ClassDesugaring where

import Frontend.Desugaring.Final.AssignmentDesugaring (desugarClassAssignments)
import Frontend.Desugaring.Final.Ast
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
    (desugaredMethods, desugaredExpressions) <- desugarClassAssignments methods
    return . Just $
        ( getValue name
        , Class
              desugaredContext
              name
              param
              desugaredMethods
              desugaredExpressions)
desugarClass _ = return Nothing
