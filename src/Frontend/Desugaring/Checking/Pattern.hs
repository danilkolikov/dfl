{- |
Module      :  Frontend.Desugaring.Checking.Pattern
Description :  Ambiguity checks for patterns
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for disambiguation of patterns
-}
module Frontend.Desugaring.Checking.Pattern where

import Control.Monad (liftM2, liftM3)
import Data.Functor (($>))
import qualified Data.HashSet as HS

import Frontend.Desugaring.Checking.Base
import Frontend.Desugaring.Grouping.Ast
import Frontend.Syntax.Position (WithLocation(..))

-- | Gets variables of a pattern
getPatternVariables :: WithLocation Pattern -> HS.HashSet Ident
getPatternVariables pat =
    case getValue pat of
        PatternInfix iPat -> getInfixPatternVariables iPat
        PatternConstr _ patterns -> HS.unions $ map getPatternVariables patterns
        PatternRecord _ bindings ->
            HS.unions $ map getPatternBindingVariables bindings
        PatternVar name maybePat ->
            HS.singleton (getValue name) `HS.union`
            maybe HS.empty getPatternVariables maybePat
        PatternConst _ -> HS.empty
        PatternWildcard -> HS.empty

-- | Gets variables of an infix pattern
getInfixPatternVariables :: WithLocation InfixPattern -> HS.HashSet Ident
getInfixPatternVariables iPat =
    case getValue iPat of
        InfixPatternApplication l _ r ->
            getInfixPatternVariables l `HS.union` getInfixPatternVariables r
        InfixPatternSimple inner -> getPatternVariables inner

-- | Gets variables of a pattern binding
getPatternBindingVariables :: WithLocation PatternBinding -> HS.HashSet Ident
getPatternBindingVariables binding =
    case getValue binding of
        PatternBinding _ pat -> getPatternVariables pat

-- | Checks idents in a pattern for ambiguity
checkPattern :: WithLocation Pattern -> CheckingProcessor (WithLocation Pattern)
checkPattern pat =
    (pat $>) <$>
    case getValue pat of
        PatternInfix iPat -> PatternInfix <$> checkInfixPattern iPat
        PatternConstr name patterns ->
            liftM2
                PatternConstr
                (checkExpressionName name)
                (mapM checkPattern patterns)
        PatternRecord name bindings ->
            liftM2
                PatternRecord
                (checkExpressionName name)
                (mapM checkPatternBinding bindings)
        PatternVar name maybePat ->
            PatternVar name <$> traverse checkPattern maybePat
        PatternConst c -> return $ PatternConst c
        PatternWildcard -> return PatternWildcard

-- | Checks idents in an infix pattern for ambiguity
checkInfixPattern ::
       WithLocation InfixPattern
    -> CheckingProcessor (WithLocation InfixPattern)
checkInfixPattern iPat =
    (iPat $>) <$>
    case getValue iPat of
        InfixPatternApplication l op r ->
            liftM3
                InfixPatternApplication
                (checkInfixPattern l)
                (checkExpressionName op)
                (checkInfixPattern r)
        InfixPatternSimple inner -> InfixPatternSimple <$> checkPattern inner

-- | Checks idents in a pattern binding for ambiguity
checkPatternBinding ::
       WithLocation PatternBinding
    -> CheckingProcessor (WithLocation PatternBinding)
checkPatternBinding binding =
    (binding $>) <$>
    case getValue binding of
        PatternBinding name pat ->
            liftM2 PatternBinding (checkExpressionName name) (checkPattern pat)
