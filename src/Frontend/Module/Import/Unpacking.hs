{- |
Module      :  Frontend.Module.Import.Unpacking
Description :  Unpacking imported definitions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for unpacking imported definitions into required data structures
-}
module Frontend.Module.Import.Unpacking
    ( unpackImports
    ) where

import Frontend.Base
import Frontend.Module.Base

-- | Function unpacks collected imports of a module into required data structures 
unpackImports :: ModuleImports -> FrontendState
unpackImports _ = emptyFrontendState
