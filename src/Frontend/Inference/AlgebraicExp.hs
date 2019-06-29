{- |
Module      :  Frontend.Inference.AlgebraicExp
Description :  Algebraic expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Algebraic expressions which can be unified
-}
module Frontend.Inference.AlgebraicExp where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)

import Frontend.Desugaring.Final.Ast (Ident(..))
import Frontend.Inference.Substitution

-- | Algebraic expressions, which support unification
data AlgebraicExp
    = AlgebraicExpVar Ident -- ^ Variable
    | AlgebraicExpFunc Ident
                       [AlgebraicExp] -- ^ Function
    deriving (Eq, Show)

instance Substitutable AlgebraicExp where
    substitute s e@(AlgebraicExpVar name) = fromMaybe e (HM.lookup name s)
    substitute s (AlgebraicExpFunc name args) =
        AlgebraicExpFunc name (map (substitute s) args)

instance WithVariables AlgebraicExp where
    getFreeVariables (AlgebraicExpVar var) = HS.singleton var
    getFreeVariables (AlgebraicExpFunc _ args) =
        HS.unions (map getFreeVariables args)

-- | Class for types which can be converted from and to AlgebraicExp
class IsAlgebraicExp a where
    toAlgebraicExp :: a -> AlgebraicExp -- ^ Convert a object to an AlgebraicExp
    fromAlgebraicExp :: AlgebraicExp -> Maybe a -- ^ Try to convert an AlgebraicExp to an object
