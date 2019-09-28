{- |
Module      :  Compiler.Module.Import
Description :  Selects objects to import
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Selects objects to import into a module
-}
module Compiler.Module.Import
    ( selectImports
    ) where

import Compiler.Module.Base
import Frontend.Processor
import Frontend.Syntax.Ast

-- | Selects objects to import into a module
selectImports ::
       ModuleProcessorState -> Module Header -> FrontendProcessorOutput
selectImports _ _ = emptyFrontendProcessorOutput
