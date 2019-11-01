{- |
Module      :  Frontend.Import.Base
Description :  Base definitions for imports selection
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base definitions, required for imports processing
-}
module Frontend.Import.Base
    ( module Frontend.Import.Base
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

-- | Imports of a module
data ModuleImports = ModuleImports
    { getModuleDataTypes :: DataTypes
    , getModuleTypeSynonyms :: Signatures [TypeSignature]
    , getModuleClasses :: Classes
    , getModuleInstances :: Instances
    , getModuleExpressions :: Expressions
    } deriving (Eq, Show)

-- | A map of data types
type DataTypes = HashMap Ident [DataType]

-- | A map of classes
type Classes = HashMap Ident [Class]

-- | A map of defined instances
type Instances = HashMap Ident [Instance]

-- | A map of expressions
type Expressions = HashMap Ident [Expression]
