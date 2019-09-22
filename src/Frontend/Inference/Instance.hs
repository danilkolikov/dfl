{- |
Module      :  Frontend.Inference.Instance
Description :  Definition of an instance of a type class
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with the definition of an instance of a type class
-}
module Frontend.Inference.Instance where

import Frontend.Desugaring.Final.Ast (Ident)
import Frontend.Inference.Constraint

-- | An instance of a type class
data Instance = Instance
    { getInstanceContext :: [SimpleConstraint] -- ^ Context of an instance
    , getInstanceClass :: Ident -- ^ Name of a type class
    , getInstanceType :: Ident -- ^ Name of a type
    , getInstanceTypeArgs :: [Ident] -- ^ Arguments of a type
    , getInstanceExpression :: Ident -- ^ An expression, generated for the instance
    , getInstanceDefaultExpression :: Ident -- ^ An expression, generated for the default instance
    } deriving (Eq, Show)
