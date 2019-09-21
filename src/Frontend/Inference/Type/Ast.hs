{- |
Module      :  Frontend.Inference.Type.Ast
Description :  Modified version of AST with type information
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Modified version of AST, produced by type inference with included type information
-}
module Frontend.Inference.Type.Ast where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)

import Frontend.Desugaring.Final.Ast (Const, Ident)
import Frontend.Inference.Signature
import Frontend.Inference.Substitution
import Frontend.Inference.WithVariables

-- | Specialisation of a previously defined expression
data External = External
    { getExternalName :: Ident -- ^ A name of the expression
    , getExternalTypeArgs :: Substitution Type -- ^ Specialised type variables
    , getExternalKindArgs :: Substitution Kind -- ^ Specialised kind variables
    } deriving (Eq, Show)

instance KindSubstitutable External where
    substituteKind sub (External name typeSub kindSub) =
        External name typeSub (HM.map (substitute sub) kindSub)

instance TypeSubstitutable External where
    substituteType sub (External name typeSub kindSub) =
        External name (HM.map (substitute sub) typeSub) kindSub

-- | An expression
data Exp
    = ExpVar Ident -- ^ Variable
    | ExpExternal External -- ^ External expression
    | ExpConstr External -- ^ Constructor
    | ExpConst Const -- ^ Constant
    | ExpAbstraction Ident
                     Exp -- ^ Lambda-abstraction
    | ExpCase Ident
              External
              [Ident]
              Exp
              Ident -- ^ Case expression
    | ExpApplication Exp
                     (NE.NonEmpty Exp) -- ^ Application of expressions
    deriving (Show, Eq)

-- | An expression with a signature
type ExpWithSignature = (Exp, TypeSignature)

-- | A map of expressions
type Expressions = HM.HashMap Ident ExpWithSignature

instance Substitutable Exp where
    substitute sub exp' =
        case exp' of
            ExpVar name -> fromMaybe exp' (HM.lookup name sub)
            ExpExternal {} -> exp'
            ExpConstr {} -> exp'
            ExpConst _ -> exp'
            ExpAbstraction name inner ->
                let withoutName = HM.delete name sub
                 in ExpAbstraction name (substitute withoutName inner)
            ExpCase x constr args ifSuccess ifFail ->
                let withoutIdents = foldr HM.delete sub (x : args)
                 in ExpCase
                        x
                        constr
                        args
                        (substitute withoutIdents ifSuccess)
                        ifFail
            ExpApplication func args ->
                ExpApplication
                    (substitute sub func)
                    (fmap (substitute sub) args)

instance WithVariables Exp where
    getVariableName exp' =
        case exp' of
            ExpVar name -> Just name
            _ -> Nothing
    getFreeVariables exp' =
        case exp' of
            ExpVar ident -> HS.singleton ident
            ExpExternal _ -> HS.empty
            ExpConstr _ -> HS.empty
            ExpConst _ -> HS.empty
            ExpAbstraction name inner -> HS.delete name (getFreeVariables inner)
            ExpCase x _ args ifSuccess _ ->
                getFreeVariables ifSuccess `HS.difference`
                HS.fromList (x : args)
            ExpApplication func args ->
                HS.unions . map getFreeVariables $ func : NE.toList args

instance SortSubstitutable Exp where
    substituteSort sub = mapExternal id (substituteSort sub)

instance KindSubstitutable Exp where
    substituteKind sub = mapExternal (substituteKind sub) (substituteKind sub)

instance TypeSubstitutable Exp where
    substituteType sub = mapExternal (substituteType sub) (substituteType sub)

-- | Maps nested External-s and TypeSignature-s
mapExternal ::
       (External -> External) -> (TypeSignature -> TypeSignature) -> Exp -> Exp
mapExternal f g exp' =
    case exp' of
        ExpVar _ -> exp'
        ExpExternal ext -> ExpExternal (f ext)
        ExpConstr ext -> ExpConstr (f ext)
        ExpConst _ -> exp'
        ExpAbstraction name inner -> ExpAbstraction name (mapExternal f g inner)
        ExpCase x ext args ifSuccess ifFail ->
            ExpCase x (f ext) args (mapExternal f g ifSuccess) ifFail
        ExpApplication func args ->
            ExpApplication (mapExternal f g func) (fmap (mapExternal f g) args)
