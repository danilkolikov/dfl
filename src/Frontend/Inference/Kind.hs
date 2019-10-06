{- |
Module      :  Frontend.Inference.Kind
Description :  Definition of a kind
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with the definition of a kind
-}
module Frontend.Inference.Kind where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Core.Ident
import Core.PredefinedIdents
import Data.Maybe (fromMaybe)
import Frontend.Inference.AlgebraicExp
import Frontend.Inference.Substitution
import Frontend.Inference.WithVariables

-- | Kind of types
data Kind
    = KindStar -- ^ Kind of a type
    | KindVar Ident -- ^ Kind variable
    | KindFunction Kind
                   Kind -- ^ Kind of a type constructor
    deriving (Eq, Show)

instance Substitutable Kind where
    substitute sub kind =
        case kind of
            KindStar -> KindStar
            KindVar ident -> fromMaybe kind (HM.lookup ident sub)
            KindFunction from to ->
                KindFunction (substitute sub from) (substitute sub to)

instance WithVariables Kind where
    getVariableName kind =
        case kind of
            KindVar name -> Just name
            _ -> Nothing
    getFreeVariables kind =
        case kind of
            KindStar -> HS.empty
            KindVar ident -> HS.singleton ident
            KindFunction from to ->
                getFreeVariables from `HS.union` getFreeVariables to

instance IsAlgebraicExp Kind where
    toAlgebraicExp kind =
        case kind of
            KindStar -> AlgebraicExpFunc (IdentUserDefined sTAR) []
            KindVar name -> AlgebraicExpVar name
            KindFunction from to ->
                AlgebraicExpFunc
                    (IdentUserDefined fUNCTION)
                    [toAlgebraicExp from, toAlgebraicExp to]
    fromAlgebraicExp aExp =
        case aExp of
            AlgebraicExpVar name -> return $ KindVar name
            AlgebraicExpFunc ident args ->
                case ident of
                    IdentUserDefined name
                        | name == sTAR -> return KindStar
                        | name == fUNCTION
                        , [from, to] <- args -> do
                            fromKind <- fromAlgebraicExp from
                            toKind <- fromAlgebraicExp to
                            return $ KindFunction fromKind toKind
                        | otherwise -> Nothing
                    _ -> Nothing
