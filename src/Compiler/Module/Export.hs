{- |
Module      :  Compiler.Module.Export
Description :  Selects objects to export
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Selects objects to export from a module
-}
module Compiler.Module.Export
    ( selectExports
    ) where

import Frontend.Desugaring.Initial.Ast
import Frontend.Processor

-- | Selects objects to import export from a module
selectExports ::
       FrontendProcessorOutput -> Header -> FrontendProcessorOutput
selectExports state _ = state
