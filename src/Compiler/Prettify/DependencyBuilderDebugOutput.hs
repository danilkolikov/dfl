{- |
Module      :  Compiler.Prettify.DependencyBuilderDebugOutput
Description :  Prettifying of DependencyBuilderDebugOutput
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of DependencyBuilderDebugOutput
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.DependencyBuilderDebugOutput where

import Compiler.Module.Base
import Compiler.Prettify.InferenceDebugOutput (prettifyGraph)
import Compiler.Prettify.Utils

instance Prettifiable DependencyBuilderDebugOutput where
    prettify DependencyBuilderDebugOutput {getDependencyBuilderDebugOutputGraph = graph} =
        unlineMaybes [prettifyGraph <$> graph]
