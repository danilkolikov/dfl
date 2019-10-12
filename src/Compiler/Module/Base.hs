{- |
Module      :  Compiler.Module.Base
Description :  Base definitions for the module processor
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base definitions for the module processor
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler.Module.Base where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)

import Compiler.Prettify.Utils
import Frontend.Processor
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

-- | A state of the module processor
type ModuleProcessorState = HM.HashMap String ModuleExports

-- | An empty state
emptyModuleProcessorState :: ModuleProcessorState
emptyModuleProcessorState = HM.empty

-- | Save a module to a state
saveModule ::
       String
    -> ModuleExports
    -> ModuleProcessorState
    -> ModuleProcessorState
saveModule = HM.insert
