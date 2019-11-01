{- |
Module      :  Frontend.Export.Base
Description :  Result of processing of a module
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Result of the processing of a module
-}
module Frontend.Export.Base
    ( module Frontend.Export.Base
    , module Frontend.Ast
    , module Core.Ident
    , module Frontend.Desugaring.Final.Ast
    , module Frontend.Inference.Instance
    ) where

import Data.HashMap.Lazy (HashMap)

import Core.Ident
import Frontend.Ast
import Frontend.Desugaring.Final.Ast (Fixity(..))
import Frontend.Inference.Instance
import Frontend.Inference.Signature
import Util.HashMap

-- | A result of module processing
data Module = Module
    { getModuleDataTypes :: DataTypes
    , getModuleTypeSynonyms :: Signatures TypeSignature
    , getModuleClasses :: Classes
    , getModuleInstances :: Instances
    , getModuleExpressions :: Expressions
    } deriving (Eq, Show)

instance Semigroup Module where
    Module d1 t1 c1 i1 e1 <> Module d2 t2 c2 i2 e2 =
        Module
            (deepMerge d1 d2)
            (t1 <> t2)
            (deepMerge c1 c2)
            (i1 <> i2)
            (e1 <> e2)

instance Monoid Module where
    mempty = Module mempty mempty mempty mempty mempty

-- | Implicit exports of a module
newtype ImplicitExport = ImplicitExport
    { getImplicitExportTypeConstructors :: Signatures TypeConstructorSignature
    } deriving (Eq, Show)

-- | Empty implicit exports
emptyImplicitExport :: ImplicitExport
emptyImplicitExport = ImplicitExport mempty

-- | All exports of a module
data ModuleExports = ModuleExports
    { getModuleExportsExplicit :: Module
    , getModuleExportsImplicit :: ImplicitExport
    } deriving (Eq, Show)

-- | Empty exports
emptyModuleExports :: ModuleExports
emptyModuleExports = ModuleExports mempty emptyImplicitExport

-- | A map of data types
type DataTypes = HashMap Ident DataType

-- | A map of classes
type Classes = HashMap Ident Class

-- | A map of defined instances
type Instances = HashMap Ident Instance

-- | A map of expressions
type Expressions = HashMap Ident Expression
