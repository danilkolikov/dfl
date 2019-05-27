{- |
Module      :  Frontend.Desugaring.ToPattern
Description :  Desugaring of AST nodes to Pattern
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Pattern-s.
-}
module Frontend.Desugaring.ToPattern
    ( DesugarToPattern(..)
    ) where

import qualified Data.List.NonEmpty as NE (toList)

import Frontend.Desugaring.Ast
import Frontend.Desugaring.ToConst (desugarToConst)
import Frontend.Desugaring.ToIdent (desugarToIdent)
import Frontend.Syntax.Ast (APat(..), FPat(..), LPat(..), Pat(..))
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Class for types which can be desugared to Pattern
class DesugarToPattern a where
    desugarToPattern :: a -> WithLocation Pattern -- ^ Desugar object to a pattern

instance (DesugarToPattern a) => DesugarToPattern (WithLocation a) where
    desugarToPattern = (getValue . desugarToPattern <$>)

instance DesugarToPattern Pat where
    desugarToPattern (PatInfix l op r) =
        withDummyLocation $
        PatternConstr
            (desugarToIdent op)
            [desugarToPattern l, desugarToPattern r]
    desugarToPattern (PatSimple p) = desugarToPattern p

instance DesugarToPattern LPat where
    desugarToPattern (LPatSimple p) = desugarToPattern p
    desugarToPattern (LPatNegated x) =
        case getValue x of
            Left int ->
                withDummyLocation .
                PatternConst . (negateConst <$>) . desugarToConst $
                int
            Right float ->
                withDummyLocation .
                PatternConst . (negateConst <$>) . desugarToConst $
                float
      where
        negateConst :: Const -> Const
        negateConst (ConstInt i) = ConstInt (-i)
        negateConst (ConstFloat f) = ConstFloat (-f)
        negateConst c = c
    desugarToPattern (LPatConstructor name args) =
        withDummyLocation $
        PatternConstr
            (desugarToIdent name)
            (map desugarToPattern (NE.toList args))

instance DesugarToPattern APat where
    desugarToPattern (APatVariable var pat) =
        withDummyLocation $
        PatternVar (desugarToIdent var) (desugarToPattern <$> pat)
    desugarToPattern (APatConstructor name) =
        withDummyLocation $ PatternConstr (desugarToIdent name) []
    desugarToPattern (APatRecord name bindings) =
        withDummyLocation $
        PatternRecord
            (desugarToIdent name)
            (map desugarToPatternBinding bindings)
    desugarToPattern (APatLiteral l) =
        withDummyLocation $ PatternConst (desugarToConst l)
    desugarToPattern APatWildcard = withDummyLocation PatternWildcard
    desugarToPattern (APatParens p) = desugarToPattern p
    desugarToPattern (APatTuple f s rest) =
        let ident = IdentParametrised tUPLE_NAME (length rest + 2)
         in withDummyLocation $
            PatternConstr
                (withDummyLocation ident)
                (map desugarToPattern (f : s : rest))
    desugarToPattern (APatList args) =
        let constr = withDummyLocation $ IdentNamed cOLON_NAME
            list = withDummyLocation $ IdentNamed lIST_NAME
            emptyList = withDummyLocation $ PatternConstr list []
         in foldr
                (\f s ->
                     withDummyLocation
                         (PatternConstr constr [desugarToPattern f, s]))
                emptyList
                args

-- Helper functions
desugarToPatternBinding :: WithLocation FPat -> WithLocation PatternBinding
desugarToPatternBinding (WithLocation (FPat name pat) loc) =
    WithLocation
        (PatternBinding (desugarToIdent name) (desugarToPattern pat))
        loc
