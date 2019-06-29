{- |
Module      :  Frontend.Inference.Unification
Description :  Unification of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for unification of expressions
-}
module Frontend.Inference.Unification where

import Data.Bifunctor (bimap)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromJust)

import Frontend.Desugaring.Final.Ast (Ident)
import Frontend.Inference.AlgebraicExp
import Frontend.Inference.Substitution

-- | Unifier of algebraic expression
type Unifier = Either UnificationError (Substitution AlgebraicExp)

-- | Errors which may be encountered during substitution
data UnificationError
    = UnificationErrorFunctionNameMismatch Ident
                                           Ident -- ^ Can't unify finctions with different names
    | UnificationErrorRecursive Ident
                                AlgebraicExp -- ^ Can't unfify recursive expressions
    | UnificationErrorDifferentNumberOfArgs -- ^ Functions have different number of args
    deriving (Eq, Show)

-- | Raise unification error
raiseError :: UnificationError -> Unifier
raiseError = Left

-- | Unify equalities between objects, which can be converted to AlgebraicExp
unifyEqualities ::
       IsAlgebraicExp a => [(a, a)] -> Either UnificationError (Substitution a)
unifyEqualities pairs =
    let equalities = map (bimap toAlgebraicExp toAlgebraicExp) pairs
        unified = unifyAlgebraicEqualities equalities
     in HM.map (fromJust . fromAlgebraicExp) <$> unified

-- | Unify a list of equalities between algebraic expressions
unifyAlgebraicEqualities :: [(AlgebraicExp, AlgebraicExp)] -> Unifier
unifyAlgebraicEqualities [] = return HM.empty
unifyAlgebraicEqualities ((first, second):rest) = do
    unified <- unifySingle first second
    let substitute' = substitute unified
        substitutedRest = map (bimap substitute' substitute') rest
    unifiedRest <- unifyAlgebraicEqualities substitutedRest
    return $ unified `compose` unifiedRest

-- | Unfify two single algebraic expressions
unifySingle :: AlgebraicExp -> AlgebraicExp -> Unifier
unifySingle (AlgebraicExpVar name) e = unifyVar name e
unifySingle e (AlgebraicExpVar name) = unifyVar name e
unifySingle (AlgebraicExpFunc name1 args1) (AlgebraicExpFunc name2 args2)
    | name1 == name2 = unifyArgs args1 args2
    | otherwise = raiseError $ UnificationErrorFunctionNameMismatch name1 name2

-- | Unify variable with an algebraic expression
unifyVar :: Ident -> AlgebraicExp -> Unifier
unifyVar name e
    | e == AlgebraicExpVar name = return HM.empty
    | e `contains` name = raiseError $ UnificationErrorRecursive name e
    | otherwise = return $ HM.singleton name e

-- | Unify two list of arguments of a function
unifyArgs :: [AlgebraicExp] -> [AlgebraicExp] -> Unifier
unifyArgs [] [] = return HM.empty
unifyArgs (e1:rest1) (e2:rest2) = do
    unifiedExps <- unifySingle e1 e2
    let substitute' = substitute unifiedExps
        substitutedRest1 = map substitute' rest1
        substitutedRest2 = map substitute' rest2
    unifiedRest <- unifyArgs substitutedRest1 substitutedRest2
    return $ unifiedExps `compose` unifiedRest
unifyArgs _ _ = raiseError UnificationErrorDifferentNumberOfArgs
