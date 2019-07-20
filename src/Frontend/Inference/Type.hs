{- |
Module      :  Frontend.Inference.Type
Description :  Definition of a type
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with the definition of a type
-}
module Frontend.Inference.Type where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE

import Data.Maybe (fromMaybe)
import Frontend.Desugaring.Final.Ast (Ident(..))
import Frontend.Inference.Substitution

-- | Type of an expression
data Type
    = TypeVar Ident -- ^ Type variable
    | TypeConstr Ident -- ^ Type constructor
    | TypeFunction Type
                   Type -- ^ Function type
    | TypeApplication Type
                      (NE.NonEmpty Type) -- ^ Application of a type constructor
    deriving (Eq, Show)

instance Substitutable Type where
    substitute sub type' =
        case type' of
            TypeVar ident -> fromMaybe type' (HM.lookup ident sub)
            TypeConstr ident -> TypeConstr ident
            TypeFunction from to ->
                TypeFunction (substitute sub from) (substitute sub to)
            TypeApplication func args ->
                TypeApplication
                    (substitute sub func)
                    (fmap (substitute sub) args)

instance WithVariables Type where
    getFreeVariables type' =
        case type' of
            TypeVar ident -> HS.singleton ident
            TypeConstr _ -> HS.empty
            TypeFunction from to ->
                getFreeVariables from `HS.union` getFreeVariables to
            TypeApplication func args ->
                HS.unions $
                getFreeVariables func : map getFreeVariables (NE.toList args)
