{- |
Module      :  Frontend.Module.Base
Description :  Result of processing of a module
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Result of the processing of a module
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Frontend.Module.Base
    ( module Frontend.Module.Base
    , module Frontend.Module.Ast
    , module Core.Ident
    , module Frontend.Desugaring.Final.Ast
    , module Frontend.Inference.Instance
    , TypeSignature
    ) where

import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)

import Core.Ident
import Frontend.Desugaring.Final.Ast (Fixity(..))
import Frontend.Inference.Instance
import Frontend.Inference.Signature
import Frontend.Module.Ast
import Util.HashMap

-- | Explicit imports or exports of a module
data Explicit = Explicit
    { getExplicitDataTypes :: DataTypes
    , getExplicitTypeSynonyms :: TypeSynonyms
    , getExplicitClasses :: Classes
    , getExplicitExpressions :: Expressions
    } deriving (Eq, Show)

instance Semigroup Explicit where
    Explicit d1 t1 c1 e1 <> Explicit d2 t2 c2 e2 =
        Explicit (deepMerge d1 d2) (t1 <> t2) (deepMerge c1 c2) (e1 <> e2)

instance Monoid Explicit where
    mempty = Explicit mempty mempty mempty mempty

-- | A map of data types
type DataTypes = HashMap Ident DataType

-- | A map of type synonyms
type TypeSynonyms = Signatures TypeSignature

-- | A map of classes
type Classes = HashMap Ident Class

-- | A map of defined instances
type Instances = HashMap Ident Instance

-- | A map of expressions
type Expressions = HashMap Ident Expression

-- | Implicit imports or exports of a module
newtype Implicit = Implicit
    { getImplicitTypeConstructors :: Signatures TypeConstructorSignature
    } deriving (Eq, Show, Semigroup, Monoid)

-- | Empty implicit exports
emptyImplicit :: Implicit
emptyImplicit = Implicit mempty

-- | All exports of a module
data ModuleExports = ModuleExports
    { getModuleExportsExplicit :: Explicit
    , getModuleExportsImplicit :: Implicit
    , getModuleExportsInstances :: Instances
    } deriving (Eq, Show)

-- | Empty exports
emptyModuleExports :: ModuleExports
emptyModuleExports = ModuleExports mempty emptyImplicit mempty

-- | All imports of a module
data ModuleImports = ModuleImports
    { getModuleImportsExplicit :: Explicit
    , getModuleImportsImplicit :: Implicit
    , getModuleImportsInstances :: Instances
    , getModuleImportsNameMapping :: NameMapping
    }

instance Semigroup ModuleImports where
    ModuleImports e1 i1 in1 nm1 <> ModuleImports e2 i2 in2 nm2 =
        ModuleImports (e1 <> e2) (i1 <> i2) (in1 <> in2) (nm1 <> nm2)

instance Monoid ModuleImports where
    mempty = ModuleImports mempty mempty mempty mempty

-- | A mapping from name to a corresponding qualified name
type NameMapping = HashMap Ident (HashSet Ident)

-- | A map of defined modules
type DefinedModules = HashMap UserDefinedIdent ModuleExports
