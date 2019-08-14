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
import Frontend.Inference.Expression
import Frontend.Inference.Signature

-- | An instance of a type class
data Instance
    = DefaultInstance { getInstanceClass :: Ident -- ^ Name of a type class
                      , getInstanceMethods :: Signatures ExpWithSignature -- ^ Methods of an instance
                       } -- ^ A default instance
    | Instance { getInstanceContext :: [SimpleConstraint] -- ^ Context of an instance
               , getInstanceClass :: Ident -- ^ Name of a type class
               , getInstanceType :: Ident -- ^ Name of a type
               , getInstanceTypeArgs :: [Ident] -- ^ Arguments of a type
               , getInstanceMethods :: Signatures ExpWithSignature -- ^ Methods of an instance
                } -- ^ A custom instance
    deriving (Eq, Show)
