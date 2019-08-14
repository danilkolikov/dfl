{- |
Module      :  Frontend.Inference.Class
Description :  Definition of a type class
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with the definition of a type class
-}
module Frontend.Inference.Class where

import Frontend.Desugaring.Final.Ast (Ident)
import Frontend.Inference.Constraint
import Frontend.Inference.Instance
import Frontend.Inference.Signature

-- | A type class
data Class = Class
    { getClassContext :: [SimpleConstraint] -- ^ Context of a type class
    , getClassName :: Ident -- ^ Name of a type class
    , getClassParam :: Ident -- ^ Parameter of a type class
    , getClassMethods :: Signatures TypeSignature -- ^ Methods of a type class
    , getClassDefaultInstance :: Maybe Instance -- ^ Default instance
    } deriving (Eq, Show)
