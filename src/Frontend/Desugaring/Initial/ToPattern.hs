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

import Core.PredefinedIdents
import Frontend.Desugaring.Initial.Ast
import Frontend.Desugaring.Initial.ToConst (desugarToConst)
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Desugaring.Initial.Utils
import Frontend.Syntax.Ast (APat(..), FPat(..), LPat(..), Pat(..))
import Frontend.Syntax.Position (WithLocation(..))

-- | Class for types which can be desugared to Pattern
class DesugarToPattern a where
    desugarToPattern :: WithLocation a -> WithLocation Pattern -- ^ Desugar object to a pattern

-- | Class for types which can be desugared to InfixPattern
class DesugarToInfixPattern a where
    desugarToInfixPattern :: WithLocation a -> WithLocation InfixPattern

instance DesugarToInfixPattern Pat where
    desugarToInfixPattern pat =
        pat $>
        case getValue pat of
            PatInfix l op r ->
                InfixPatternApplication
                    (desugarToInfixPattern l)
                    (desugarToIdent op)
                    (desugarToInfixPattern r)
            PatSimple p -> InfixPatternSimple $ desugarToPattern p

instance DesugarToPattern Pat where
    desugarToPattern pat = pat $> PatternInfix (desugarToInfixPattern pat)

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
                let ident = makeIdent $ tUPLE (length rest + 2)
                 in PatternConstr ident (map desugarToPattern (f : s : rest))
            APatList (f NE.:| rest) ->
                let constr = makeIdent cOLON
                    firstDesugared = desugarToPattern f
                    patsDesugared =
                        case rest of
                            [] -> makePattern lIST
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
