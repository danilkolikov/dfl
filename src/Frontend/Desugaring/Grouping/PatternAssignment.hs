{- |
Module      :  Frontend.Desugaring.Grouping.PatternAssignment
Description :  Grouping of pattern assignments
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Grouping of pattern assignments
-}
module Frontend.Desugaring.Grouping.PatternAssignment where

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes, isJust, mapMaybe)

import Frontend.Desugaring.Grouping.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Desugar a single pattern assignment
groupSinglePattern ::
       (WithLocation Pattern, WithLocation Exp)
    -> [(WithLocation Ident, WithLocation Exp)]
groupSinglePattern (singlePattern, exp')
      -- Special case of a function without arguments
    | (PatternVar name Nothing) <- getValue singlePattern = [(name, exp')]
    | otherwise =
        case getVariablesFromPattern singlePattern of
            [] -> []
            patterns ->
                let idents = map getValue . mapMaybe snd $ patterns
                    expIdent =
                        withDummyLocation . IdentGenerated $
                        GeneratedIdentGroup idents
                    expPair = (expIdent, exp')
                    expVar = expIdent $> ExpVar expIdent
                    prepared = map (processPattern expVar) patterns
                 in expPair : catMaybes prepared

-- | Create an binding for a single variable from a pattern
processPattern ::
       WithLocation Exp
    -> (WithLocation Pattern, Maybe (WithLocation Ident))
    -> Maybe (WithLocation Ident, WithLocation Exp)
processPattern _ (_, Nothing) = Nothing
processPattern expVar (pat, Just var) =
    let varExp = withDummyLocation $ ExpVar var
        alt = withDummyLocation $ AltSimple pat varExp
        case' = withDummyLocation $ ExpCase expVar (alt NE.:| [])
     in Just (var, case')

-- | Remove variables from a pattern
patternWithoutVariables :: WithLocation Pattern -> WithLocation Pattern
patternWithoutVariables pat =
    case getValue pat of
        PatternWildcard -> pat
        PatternConst {} -> pat
        PatternVar _ Nothing -> pat $> PatternWildcard
        PatternVar _ (Just p) -> patternWithoutVariables p
        PatternConstr name args ->
            pat $> PatternConstr name (map patternWithoutVariables args)
        PatternInfix inf ->
            pat $> PatternInfix (infixPatternWithoutVariables inf)
        PatternRecord name bindings ->
            pat $>
            PatternRecord name (fmap patternBindingWithoutVariables bindings)

-- | Creates an infix pattern without variables
infixPatternWithoutVariables ::
       WithLocation InfixPattern -> WithLocation InfixPattern
infixPatternWithoutVariables pat =
    pat $>
    case getValue pat of
        InfixPatternApplication l op r ->
            InfixPatternApplication
                (infixPatternWithoutVariables l)
                op
                (infixPatternWithoutVariables r)
        InfixPatternSimple inner ->
            InfixPatternSimple (patternWithoutVariables inner)

-- | Creates a pattern binding without variables
patternBindingWithoutVariables ::
       WithLocation PatternBinding -> WithLocation PatternBinding
patternBindingWithoutVariables pat =
    pat $>
    case getValue pat of
        PatternBinding name inner ->
            PatternBinding name (patternWithoutVariables inner)

-- | Find all variables in a pattern, and for each of them create a pattern,
-- | where other variables are removed
getVariablesFromPattern ::
       WithLocation Pattern
    -> [(WithLocation Pattern, Maybe (WithLocation Ident))]
getVariablesFromPattern pat =
    case getValue pat of
        PatternWildcard -> [(pat, Nothing)]
        PatternConst {} -> [(pat, Nothing)]
        PatternVar name Nothing -> [(pat, Just name)]
        PatternVar name (Just p) ->
            let variables = getVariablesFromPattern p
             in ( pat $> PatternVar name (Just $ patternWithoutVariables p)
                , Just name) :
                variables
        PatternConstr name args ->
            getVariablesFromConstructor
                getVariablesFromPattern
                patternWithoutVariables
                ((pat $>) . PatternConstr name)
                args
        PatternInfix infixPattern ->
            let infixPatterns = getVariablesFromInfixPattern infixPattern
                makePattern (iPat, res) = (pat $> PatternInfix iPat, res)
             in map makePattern infixPatterns
        PatternRecord name bindings ->
            getVariablesFromConstructor
                getVariablesFromPatternBinding
                patternBindingWithoutVariables
                ((pat $>) . PatternRecord name)
                bindings

-- | Gets variables from an infix pattern
getVariablesFromInfixPattern ::
       WithLocation InfixPattern
    -> [(WithLocation InfixPattern, Maybe (WithLocation Ident))]
getVariablesFromInfixPattern infixPat =
    case getValue infixPat of
        InfixPatternSimple pat ->
            let patterns = getVariablesFromPattern pat
                makeInfixPattern (inner, res) =
                    (infixPat $> InfixPatternSimple inner, res)
             in map makeInfixPattern patterns
        InfixPatternApplication l op r ->
            getVariablesFromConstructor
                getVariablesFromInfixPattern
                infixPatternWithoutVariables
                (\[left, right] ->
                     infixPat $> InfixPatternApplication left op right)
                [l, r]

-- | Gets variables from a pattern binding
getVariablesFromPatternBinding ::
       WithLocation PatternBinding
    -> [(WithLocation PatternBinding, Maybe (WithLocation Ident))]
getVariablesFromPatternBinding binding =
    case getValue binding of
        PatternBinding name pat ->
            let patterns = getVariablesFromPattern pat
                makeBinding (p, res) = (binding $> PatternBinding name p, res)
             in map makeBinding patterns

-- | Gets variables from an abstract constructor
getVariablesFromConstructor ::
       (a -> [(a, Maybe (WithLocation Ident))])
    -> (a -> a)
    -> ([a] -> b)
    -> [a]
    -> [(b, Maybe (WithLocation Ident))]
getVariablesFromConstructor getVariables getWithoutVariables wrapResult args =
    let variables = fmap getVariables args
        withoutVariables = map getWithoutVariables args
        prepared = zip variables withoutVariables
        makePatterns _ [] = []
        makePatterns prev ((vars, withoutVars):rest) =
            let madeRest = makePatterns (prev ++ [withoutVars]) rest
                restWithoutVariables = map snd rest
                resPatterns =
                    [ (wrapResult newArgs, var)
                    | (single, var) <- vars
                    , let newArgs = prev ++ [single] ++ restWithoutVariables
                    ]
             in resPatterns ++ madeRest
        res = makePatterns [] prepared
     in filter (isJust . snd) res
