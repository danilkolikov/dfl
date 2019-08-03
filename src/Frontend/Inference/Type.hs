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
import Frontend.Inference.AlgebraicExp
import Frontend.Inference.Expression
import Frontend.Inference.Substitution
import Frontend.Syntax.EntityName

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
    getVariableName type' =
        case type' of
            TypeVar name -> Just name
            _ -> Nothing
    getFreeVariables type' =
        case type' of
            TypeVar ident -> HS.singleton ident
            TypeConstr _ -> HS.empty
            TypeFunction from to ->
                getFreeVariables from `HS.union` getFreeVariables to
            TypeApplication func args ->
                HS.unions $
                getFreeVariables func : map getFreeVariables (NE.toList args)

instance IsAlgebraicExp Type where
    toAlgebraicExp kind =
        case kind of
            TypeVar name -> AlgebraicExpVar name
            TypeConstr name -> AlgebraicExpFunc name []
            TypeFunction from to ->
                AlgebraicExpFunc
                    (IdentNamed aPPLICATION_NAME)
                    [ AlgebraicExpFunc (IdentNamed fUNCTION_NAME) []
                    , toAlgebraicExp from
                    , toAlgebraicExp to
                    ]
            TypeApplication func args ->
                AlgebraicExpFunc
                    (IdentNamed aPPLICATION_NAME)
                    (toAlgebraicExp func : map toAlgebraicExp (NE.toList args))
    fromAlgebraicExp aExp =
        case aExp of
            AlgebraicExpVar name -> return $ TypeVar name
            AlgebraicExpFunc ident args
                | [] <- args -> return $ TypeConstr ident
                | func:rest <- args
                , IdentNamed name <- ident
                , name == aPPLICATION_NAME -> do
                    funcType <- fromAlgebraicExp func
                    restType <- mapM fromAlgebraicExp rest
                    if funcType == TypeConstr (IdentNamed fUNCTION_NAME) &&
                       length restType == 2
                        then return $
                             TypeFunction (head restType) (last restType)
                        else case restType of
                                 [] -> Nothing
                                 hd:tl ->
                                     return $
                                     TypeApplication funcType (hd NE.:| tl)
                | otherwise -> Nothing
