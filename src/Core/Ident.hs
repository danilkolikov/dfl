{- |
Module      :  Core.Ident
Description :  Definition of identifiers
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Identifiers, used in programs
-}
{-# LANGUAGE DeriveGeneric #-}

module Core.Ident where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- | A name of an identifier
type IdentName = String

-- | An identifier in a program
data Ident
    = IdentUserDefined UserDefinedIdent -- ^ A user-defined identifier
    | IdentGenerated GeneratedIdent -- ^ A generated identifier
    deriving (Generic, Eq, Ord, Show)

instance Hashable Ident

-- | A user-defined identifier
data UserDefinedIdent
    = IdentQualified [IdentName]
                     SimpleIdent -- ^ A qualified identifier
    | IdentSimple SimpleIdent -- ^ A simple identifier
    deriving (Generic, Eq, Ord, Show)

instance Hashable UserDefinedIdent

-- | A non-qualified identifier
data SimpleIdent
    = IdentNamed IdentName -- ^ A single name
    | IdentParametrised IdentName
                        Int -- ^ A parameterised name
    deriving (Generic, Eq, Ord, Show)

instance Hashable SimpleIdent

-- | A generated identifier
data GeneratedIdent
    = GeneratedIdent GeneratedIdentEnvironment
                     Int -- ^ Simply generated identifier
    | GeneratedIdentGroup [Ident] -- ^ A name of a group of identifiers
    | GeneratedIdentScoped [Ident] -- ^ A scoped identifier
    | GeneratedIdentInstance Ident
                             Ident -- ^ An identifier for an instance
    deriving (Generic, Eq, Ord, Show)

instance Hashable GeneratedIdent

-- | Sets of generated identifiers
data GeneratedIdentEnvironment
    = GeneratedIdentEnvironmentGrouping -- ^ Identifiers for expression grouping
    | GeneratedIdentEnvironmentRecordDesugaring -- ^ Identifiers for record desugaring
    | GeneratedIdentEnvironmentExpressionDesugaring -- ^ Indentifiers for xpression desugaring
    | GeneratedIdentEnvironmentTypeVariable -- ^ Type variables
    | GeneratedIdentEnvironmentKindVariable -- ^ Kind variables
    | GeneratedIdentEnvironmentSortVariable -- ^ Sort variables
    | GeneratedIdentEnvironmentInstances -- ^ Identifiers for inference of instances
    | GeneratedIdentEnvironmentLet -- ^ Identifiers for desugaring of let expressions
    | GeneratedIdentEnvironmentTranslation -- ^ Identifiers for translation of expressions
    deriving (Generic, Eq, Ord, Show)

instance Hashable GeneratedIdentEnvironment
