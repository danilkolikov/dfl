{- |
Module      :  Frontend.Desugaring.ToClassAssignment
Description :  Desugaring of AST nodes to ClassAssignment
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing ClassAssignment-s.
-}
module Frontend.Desugaring.ToClassAssignment where

import qualified Frontend.Desugaring.Ast as D
import Frontend.Desugaring.IdentGenerator (IdentGenerator)
import Frontend.Desugaring.ToExp (desugarToExp)
import Frontend.Desugaring.ToIdent (desugarToIdent)
import Frontend.Desugaring.Util (desugarFunLHS, desugarGenDecl, rhsToExp)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Class for types which can be desugared to ClassAssignment
class DesugarToClassAssignment a where
    desugarToClassAssignment ::
           a -> IdentGenerator [WithLocation D.ClassAssignment]-- ^ Desugar object to ClassAssignment-s

instance (DesugarToClassAssignment a) =>
         DesugarToClassAssignment (WithLocation a) where
    desugarToClassAssignment x = do
        res <- desugarToClassAssignment (getValue x)
        return [getValue el <$ x | el <- res]

instance DesugarToClassAssignment CDecl where
    desugarToClassAssignment (CDeclGenDecl genDecl) =
        desugarToClassAssignment genDecl
    desugarToClassAssignment (CDeclFunction lhs rhs) =
        case getValue lhs of
            Left fun -> do
                let (ident, pats) = desugarFunLHS fun
                    exp' = getValue <$> (rhsToExp <$> rhs)
                res <- desugarToExp $ LExpAbstraction pats exp'
                return [withDummyLocation $ D.ClassAssignmentName ident res]
            Right var ->
                return .
                withDummyLocation .
                D.ClassAssignmentName (desugarToIdent (var <$ lhs)) <$>
                desugarToExp rhs

instance DesugarToClassAssignment GenDecl where
    desugarToClassAssignment = return . desugarGenDecl D.ClassAssignmentType
