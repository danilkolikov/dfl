{- |
Module      :  Frontend.Inference.Class
Description :  Definition of a type class
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with the definition of a type class
-}
module Frontend.Inference.Class where

import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Final.Ast (Ident)
import Frontend.Inference.Constraint
import Frontend.Inference.Kind.Ast (Exp)
import Frontend.Syntax.Position (WithLocation)

-- | Definition of a type class
data Class = Class
    { getClassContext :: [SimpleConstraint] -- ^ Context of a type class
    , getClassName :: Ident -- ^ Name of a type class
    , getClassParam :: Ident -- ^ Parameter of a type class
    , getClassDataTypeName :: Ident -- ^ Ident of the generated data type
    , getClassGetters :: HM.HashMap Ident Ident -- ^ Getters for methods of this and superclasses
    , getClassMethods :: [Ident] -- ^ List of methods
    , getClassDefaultInstanceName :: Ident -- ^ Ident of the generated default instance
    } deriving (Eq, Show)

-- | A default instance of a type class
data DefaultInstance = DefaultInstance
    { getDefaultInstanceClassName :: Ident -- ^ A name of the class
    , getDefaultInstanceParam :: Ident -- ^ A parameter of the instance
    , getDefaultInstanceMethods :: HM.HashMap Ident (WithLocation Exp) -- ^ Implementations of methods
    } deriving (Eq, Show)
