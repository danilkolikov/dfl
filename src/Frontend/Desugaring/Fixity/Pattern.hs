{- |
Module      :  Frontend.Desugaring.Fixity.Pattern
Description :  Resolution of fixity of patterns
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Fixity resolution of expressions in DFL
-}
module Frontend.Desugaring.Fixity.Pattern
    ( resolvePattern
    ) where

import Data.Functor (($>))

import Frontend.Desugaring.Fixity.Ast
import Frontend.Desugaring.Fixity.Base
import Frontend.Desugaring.Fixity.Resolution
import qualified Frontend.Desugaring.Grouping.Ast as G
import Frontend.Syntax.Position (WithLocation(..))

-- | Resolves fixity of a pattern
resolvePattern ::
       WithLocation G.Pattern -> FixityResolver (WithLocation Pattern)
resolvePattern pat =
    (pat $>) <$>
    case getValue pat of
        G.PatternInfix iPat -> resolveInfixPattern iPat
        G.PatternConstr name patterns ->
            PatternConstr name <$> mapM resolvePattern patterns
        G.PatternRecord name bindings ->
            PatternRecord name <$> mapM resolvePatternBinding bindings
        G.PatternVar name maybePat ->
            PatternVar name <$> traverse resolvePattern maybePat
        G.PatternConst c -> return $ PatternConst c
        G.PatternWildcard -> return PatternWildcard

resolveInfixPattern :: WithLocation G.InfixPattern -> FixityResolver Pattern
resolveInfixPattern iPat = collectFlatInfix iPat >>= resolveFixity
  where
    collectFlatInfix ::
           WithLocation G.InfixPattern -> FixityResolver [FlatInfix Pattern]
    collectFlatInfix pat =
        case getValue pat of
            G.InfixPatternSimple inner -> do
                innerResolved <- resolvePattern inner
                return [FlatInfixExp innerResolved]
            G.InfixPatternApplication l op r -> do
                lCollected <- collectFlatInfix l
                rCollected <- collectFlatInfix r
                return $ lCollected ++ [FlatInfixOp op] ++ rCollected

resolvePatternBinding ::
       WithLocation G.PatternBinding
    -> FixityResolver (WithLocation PatternBinding)
resolvePatternBinding binding =
    (binding $>) <$>
    case getValue binding of
        G.PatternBinding name pat -> PatternBinding name <$> resolvePattern pat
