{- |
Module      :  Frontend.Inference.Sort
Description :  Definition of a sort
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with the definition of a sort
-}
module Frontend.Inference.Sort where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)

import Core.Ident
import Core.PredefinedIdents
import Frontend.Inference.AlgebraicExp
import Frontend.Inference.WithVariables
import Frontend.Inference.Substitution

-- | Sort of kinds
data Sort
    = SortSquare -- ^ Sort of a kind
    | SortVar Ident -- ^ Sort variable
    | SortFunction Sort
                   Sort -- ^ Sort of a kind constructor
    deriving (Eq, Show)

instance Substitutable Sort where
    substitute sub kind =
        case kind of
            SortSquare -> SortSquare
            SortVar ident -> fromMaybe kind (HM.lookup ident sub)
            SortFunction from to ->
                SortFunction (substitute sub from) (substitute sub to)

instance WithVariables Sort where
    getVariableName sort =
        case sort of
            SortVar name -> Just name
            _ -> Nothing
    getFreeVariables kind =
        case kind of
            SortSquare -> HS.empty
            SortVar ident -> HS.singleton ident
            SortFunction from to ->
                getFreeVariables from `HS.union` getFreeVariables to

instance IsAlgebraicExp Sort where
    toAlgebraicExp kind =
        case kind of
            SortSquare -> AlgebraicExpFunc (IdentUserDefined lIST) []
            SortVar name -> AlgebraicExpVar name
            SortFunction from to ->
                AlgebraicExpFunc
                    (IdentUserDefined fUNCTION)
                    [toAlgebraicExp from, toAlgebraicExp to]
    fromAlgebraicExp aExp =
        case aExp of
            AlgebraicExpVar name -> return $ SortVar name
            AlgebraicExpFunc ident args ->
                case ident of
                    IdentUserDefined name
                        | name == lIST -> return SortSquare
                        | name == fUNCTION
                        , [from, to] <- args -> do
                            fromSort <- fromAlgebraicExp from
                            toSort <- fromAlgebraicExp to
                            return $ SortFunction fromSort toSort
                        | otherwise -> Nothing
                    _ -> Nothing
