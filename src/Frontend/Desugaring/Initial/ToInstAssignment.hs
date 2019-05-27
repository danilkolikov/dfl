{- |
Module      :  Frontend.Desugaring.Initial.ToInstAssignment
Description :  Desugaring of AST nodes to InstAssignment
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing InstAssignment-s.
-}
module Frontend.Desugaring.Initial.ToInstAssignment
    ( DesugarToInstAssignment(..)
    ) where

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToExp (desugarToExp)
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Desugaring.Initial.Util (desugarFunLHS, rhsToExp)
import Frontend.Desugaring.Util.IdentGenerator (IdentGenerator)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Class for types which can be desugared to InstAssignment
class DesugarToInstAssignment a where
    desugarToInstAssignment ::
           a -> IdentGenerator (WithLocation D.InstAssignment) -- Desugar object to InstAssignment

instance (DesugarToInstAssignment a) =>
         DesugarToInstAssignment (WithLocation a) where
    desugarToInstAssignment =
        sequence . ((getValue <$>) . desugarToInstAssignment <$>)

instance DesugarToInstAssignment IDecl where
    desugarToInstAssignment (IDeclFunction lhs rhs) =
        case getValue lhs of
            Left fun -> do
                let (ident, pats) = desugarFunLHS fun
                    exp' = getValue <$> (rhsToExp <$> rhs)
                res <- desugarToExp $ LExpAbstraction pats exp'
                return . withDummyLocation $ D.InstAssignmentName ident res
            Right var ->
                withDummyLocation .
                D.InstAssignmentName (desugarToIdent (var <$ lhs)) <$>
                desugarToExp rhs
