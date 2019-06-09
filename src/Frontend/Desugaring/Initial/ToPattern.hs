{- |
Module      :  Frontend.Desugaring.Initial.ToPattern
Description :  Desugaring of AST nodes to Pattern
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Pattern-s.
-}
module Frontend.Desugaring.Initial.ToPattern
    ( DesugarToPattern(..)
    , desugarToPatternBinding
    ) where

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE (NonEmpty(..), toList)

import Frontend.Desugaring.Initial.Ast
import Frontend.Desugaring.Initial.ToConst (desugarToConst)
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Desugaring.Initial.Util
import Frontend.Syntax.Ast (APat(..), FPat(..), LPat(..), Pat(..))
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..))

-- | Class for types which can be desugared to Pattern
class DesugarToPattern a where
    desugarToPattern :: WithLocation a -> WithLocation Pattern -- ^ Desugar object to a pattern

instance DesugarToPattern Pat where
    desugarToPattern pat =
        case getValue pat of
            PatInfix l op r ->
                pat $>
                PatternConstr
                    (desugarToIdent op)
                    [desugarToPattern l, desugarToPattern r]
            PatSimple p -> desugarToPattern p

instance DesugarToPattern LPat where
    desugarToPattern lPat =
        case getValue lPat of
            LPatSimple p -> desugarToPattern p
            LPatNegated x ->
                let negateConst :: Const -> Const
                    negateConst (ConstInt i) = ConstInt (-i)
                    negateConst (ConstFloat f) = ConstFloat (-f)
                    negateConst c = c
                 in lPat $>
                    case getValue x of
                        Left int ->
                            PatternConst . (negateConst <$>) . desugarToConst $
                            x $> int
                        Right float ->
                            PatternConst . (negateConst <$>) . desugarToConst $
                            x $> float
            LPatConstructor name args ->
                lPat $>
                PatternConstr
                    (desugarToIdent name)
                    (map desugarToPattern (NE.toList args))

instance DesugarToPattern APat where
    desugarToPattern aPat =
        aPat $>
        case getValue aPat of
            APatVariable var pat ->
                PatternVar (desugarToIdent var) (desugarToPattern <$> pat)
            APatConstructor name -> PatternConstr (desugarToIdent name) []
            APatRecord name bindings ->
                PatternRecord
                    (desugarToIdent name)
                    (map desugarToPatternBinding bindings)
            APatLiteral l -> PatternConst (desugarToConst l)
            APatWildcard -> PatternWildcard
            APatParens p -> getValue $ desugarToPattern p
            APatTuple f s rest ->
                let ident =
                        makeIdent' $
                        IdentParametrised tUPLE_NAME (length rest + 2)
                 in PatternConstr ident (map desugarToPattern (f : s : rest))
            APatList (f NE.:| rest) ->
                let constr = makeIdent cOLON_NAME
                    firstDesugared = desugarToPattern f
                    patsDesugared =
                        case rest of
                            [] -> makePattern lIST_NAME
                            second:pats ->
                                let list = aPat $> APatList (second NE.:| pats)
                                 in desugarToPattern list
                 in PatternConstr constr [firstDesugared, patsDesugared]

-- Helper functions
-- | Desugar FPat to a pattern binding
desugarToPatternBinding :: WithLocation FPat -> WithLocation PatternBinding
desugarToPatternBinding (WithLocation (FPat name pat) loc) =
    WithLocation
        (PatternBinding (desugarToIdent name) (desugarToPattern pat))
        loc
