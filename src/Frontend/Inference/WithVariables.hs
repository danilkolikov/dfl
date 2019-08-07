{- |
Module      :  Frontend.Inference.WithVariables
Description :  Interface for expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions which are common for all expressions (expressions, types, kinds, sorts)
-}
module Frontend.Inference.WithVariables where

import qualified Data.HashSet as HS

import Frontend.Desugaring.Final.Ast (Ident(..))

-- | A class of expressions which have variables
class WithVariables a where
    getVariableName :: a -> Maybe Ident -- ^ Get the name of a variable
    getFreeVariables :: a -> HS.HashSet Ident -- ^ Get the set of free variables

-- | Check if the object freely contains the provided variable
contains :: (WithVariables a) => a -> Ident -> Bool
contains x name = HS.member name (getFreeVariables x)
