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

import Frontend.Processor
import Frontend.Syntax.Ast

-- | Selects objects to import export from a module
selectExports ::
       FrontendProcessorOutput -> Module Header -> FrontendProcessorOutput
selectExports state _ = state
