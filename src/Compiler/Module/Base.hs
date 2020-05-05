{- |
Module      :  Compiler.Module.Base
Description :  Base definitions for the module processor
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base definitions for the module processor
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler.Module.Base
    ( module Compiler.Module.Base
    , DefinedModules
    ) where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Frontend.Module.Base

import Compiler.Prettify.Utils
import Util.DependencyResolver

-- | A type of errors which can be encountered during
data DependencyBuilderError
    = DependencyBuilderErrorMissingFile String -- ^ A missing module
    | DependencyBuilderErrorCycle [String] -- ^ Cycle in module hierarchy
    | DependencyBuilderErrorMismatchingModuleName String -- ^ Mismatching name of a module
    deriving (Eq, Show)

-- | A debug output of a dependency builder
newtype DependencyBuilderDebugOutput = DependencyBuilderDebugOutput
    { getDependencyBuilderDebugOutputGraph :: Maybe (DependencyGraph FileName)
    }

instance Semigroup DependencyBuilderDebugOutput where
    DependencyBuilderDebugOutput g1 <> DependencyBuilderDebugOutput g2 =
        DependencyBuilderDebugOutput (g1 <|> g2)

instance Monoid DependencyBuilderDebugOutput where
    mempty = DependencyBuilderDebugOutput Nothing

-- | A name of a file
newtype FileName = FileName
    { getFileName :: String
    } deriving (Eq, Hashable)

instance Prettifiable FileName where
    prettify = show . getFileName

-- | An empty set of modules
emptyDefinedModules :: DefinedModules
emptyDefinedModules = HM.empty

-- | Save a module to a state
defineModule ::
       UserDefinedIdent -> ModuleExports -> DefinedModules -> DefinedModules
defineModule = HM.insert
